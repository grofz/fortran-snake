module snake_mod
    use raylib
    use iso_c_binding, only : c_null_char, c_double
    use iso_fortran_env, only : int64
    implicit none (type, external)

!integer :: leak_check_counter = 0

    integer, parameter :: WINDOW_WIDTH=800, WINDOW_HEIGHT=600, PIXEL_SIZE=10
    integer, parameter :: WINDOW_TOP_MARGIN=30
    integer, parameter :: MAP_WIDTH=WINDOW_WIDTH/PIXEL_SIZE
    integer, parameter :: MAP_HEIGHT=(WINDOW_HEIGHT-WINDOW_TOP_MARGIN)/PIXEL_SIZE
    integer, parameter :: TARGET_FPS=60
    real(c_double), parameter :: UPDATE_TSTEP=0.1_c_double ! seconds
    integer, parameter :: NUMBER_OF_SNAKES=8
    integer, parameter :: NUMBER_OF_FOOD=int(MAP_WIDTH*MAP_HEIGHT*0.021)
    integer, parameter :: AI_SIGHT_RANGE = max(MAP_WIDTH/4,5)

    type(color_type), parameter :: PALETTE(*) = [ &
    & BLACK, BEIGE, LIME, GOLD, PINK, MAROON, SKYBLUE, DARKGRAY, GREEN, DARKGREEN, BLUE, VIOLET]

    integer, parameter :: ID_FREE=0, ID_FOOD=-2
    integer, parameter :: STATE_GAME=0, STATE_END=1
    ! Left, Down, Right, Up
    integer, parameter :: DIR_LEFT=1, DIR_DOWN=2, DIR_RIGHT=3, DIR_UP=4
    integer, parameter :: DIRS(2,4) = reshape([-1, 0, 0, 1, 1, 0, 0, -1], shape=[2,4])

    type game_t
        integer :: map(MAP_WIDTH, MAP_HEIGHT)
        integer :: state, snake_counter
    end type game_t

! SNAKE
! head <- slice3 <- slice2 <- tail
    type snakeslice_t
        integer :: x(2)
        type(snakeslice_t), pointer :: prev => null()
    end type snakeslice_t

    type snake_t
        type(snakeslice_t), pointer :: head => null()
        type(snakeslice_t), pointer :: tail => null()
        integer :: dir = 3
        logical :: is_growing = .false.
        logical :: is_alive = .true.
        integer :: length = 1
        integer :: ai_agent = 1
        integer :: id
    contains
        final :: free_snake
    end type

contains

    subroutine initialize(snakes, game)
        type(snake_t), intent(inout) :: snakes(:)
        type(game_t), intent(inout) :: game
        integer :: tmp, i

        game%state = STATE_GAME
        game%map = ID_FREE
        game%snake_counter = 0
        do i=1,NUMBER_OF_SNAKES
            call new_snake(snakes(i), game%snake_counter, game%map)
            ! TODO verify if snake could not be placed
        end do
       !snakes(1)%ai_agent = 0 ! manual control of snake 1
        do tmp=1, NUMBER_OF_FOOD
            call grow_food(game%map)
        end do
    end subroutine initialize


    subroutine main_loop(snakes, game)
        type(snake_t), intent(inout) :: snakes(:)
        type(game_t), intent(inout) :: game

        integer :: collision(NUMBER_OF_SNAKES), i
        character(len=:), allocatable :: text_buff

        select case(game%state)
        case(STATE_GAME)
            call mancontrol_snake(snakes(1))
            collision = ID_FREE
            TSTEP: if (is_time_to_update()) then
                do i=1,NUMBER_OF_SNAKES
                    call aicontrol_snake(snakes(i), game)
                end do
                do i=1,NUMBER_OF_SNAKES
                    call move_snake(snakes(i), game%map, collision(i))
                end do
                ! grow snakes that have eaten food
                do i=1,NUMBER_OF_SNAKES
                    if (collision(i)==ID_FOOD) then
                        call grow_snake(snakes(i))
                        call grow_food(game%map)
                    end if
                end do
                ! resolve collisions
                do i=1,NUMBER_OF_SNAKES
                    if (collision(i) <= ID_FREE) cycle
                    snakes(i)%is_alive = .false.
                    associate(other_snake=>snakes(collision(i)))
                        ! snake collides into the tail that will be removed in the next frame
                        if (other_snake%id>i .and. .not. other_snake%is_growing .and. &
                            & all(other_snake%tail%x==snakes(i)%head%x)) then
                            snakes(i)%is_alive = .true.
print '("Close call for ",i0," bumping into ",i0,"s tail")', i, other_snake%id
                        elseif (other_snake%id==i) then
print '("Snake ",i0," killed itself")', i
                        elseif (all(other_snake%head%x==snakes(i)%head%x)) then
print '("Head on collision of ",i0," with ",i0)', i, other_snake%id
                        else
print '("Snake ",i0," killed by ",i0)', i, other_snake%id
                        end if

                        ! head on with other snake (correct that other snake did not collide)
                        if (other_snake%id<i .and. all(other_snake%head%x==snakes(i)%head%x)) then
                            other_snake%is_alive = .false.
                            ! food claimed by two snakes during head on collision
                            if (other_snake%is_growing) other_snake%length=other_snake%length-1
print '("Head on collision of ",i0," with ",i0)', other_snake%id, i
                        end if
                    end associate
                end do
                do i=1,NUMBER_OF_SNAKES
                    snakes(i)%is_growing = .false.
                end do
            end if TSTEP

            ! end if all is dead
            if (count(snakes%is_alive)==0) game%state = STATE_END

        case(STATE_END)
            continue
        case default
            error stop 'invalid state'
        end select

        ! manual restart
        if (is_key_pressed(KEY_R)) then
            call initialize(snakes, game)
            print '(/,"Game restarted")'
        end if

        call begin_drawing()
            call clear_background(RAYWHITE)
            call render_map(game%map, snakes)
            call draw_line(0, WINDOW_TOP_MARGIN, WINDOW_WIDTH, WINDOW_TOP_MARGIN, BLACK)
            call draw_line(0, WINDOW_TOP_MARGIN+WINDOW_HEIGHT, WINDOW_WIDTH, WINDOW_TOP_MARGIN+WINDOW_HEIGHT, BLACK)
            call render_score(snakes)
            if (game%state==STATE_END) then
                text_buff = 'PRESS "R" TO RESTART'//c_null_char
                associate (fs=>35)
                    call draw_text(text_buff, &
                    &   WINDOW_WIDTH/2-measure_text(text_buff, fs)/2, &
                    &   WINDOW_TOP_MARGIN+(WINDOW_HEIGHT-WINDOW_TOP_MARGIN)/2-fs/2, fs, BLACK)
                end associate
            end if
            !call draw_fps(int(0.75*WINDOW_WIDTH), 5)
        call end_drawing()
    end subroutine main_loop


    subroutine render_map(map, snakes)
        integer, intent(in) :: map(:,:)
        type(snake_t), intent(in) :: snakes(:)
        integer :: x, y, wx, wy, id_color, i
        do x=1, MAP_WIDTH
            do y=1,MAP_HEIGHT
                wx = (x-1)*PIXEL_SIZE
                wy = (y-1)*PIXEL_SIZE + WINDOW_TOP_MARGIN
                select case(map(x,y))
                case default
                    if (map(x,y)<=0 .or. map(x,y)>size(snakes)) error stop 'invalid value in map'
                    id_color = mod(map(x,y)-1, size(PALETTE))+1
                    if (any(snakes(map(x,y))%head%x/=[x,y])) then
                        !call draw_rectangle(wx, wy, PIXEL_SIZE, PIXEL_SIZE, PALETTE(id_color))
                        call draw_rectangle_rounded( &
                        &  rectangle_type(wx, wy, PIXEL_SIZE, PIXEL_SIZE), &
                        &  0.5, 5, PALETTE(id_color))
                    end if
                case (ID_FOOD)
                    call draw_circle(int(wx+0.5*PIXEL_SIZE), int(wy+0.5*PIXEL_SIZE), 0.5*real(PIXEL_SIZE), GREEN)

                case (ID_FREE)
                end select
            end do
        end do

        ! render heads separately
        do i=1,size(snakes)
            wx = (snakes(i)%head%x(1)-1)*PIXEL_SIZE
            wy = (snakes(i)%head%x(2)-1)*PIXEL_SIZE + WINDOW_TOP_MARGIN
            id_color = mod(snakes(i)%id-1, size(PALETTE))+1
            call render_snake_head(wx, wy, snakes(i)%dir, id_color)
        end do
    end subroutine render_map


    subroutine render_snake_head(wx, wy, dir, id_color)
        integer, intent(in) :: wx, wy, dir, id_color
        type(vector2_type) :: v1, v2, v3

        select case(dir)
        case(DIR_RIGHT)
            v1 = vector2_type(wx, wy) ! left top
            v2 = vector2_type(wx, wy+PIXEL_SIZE)
            v3 = vector2_type(wx+PIXEL_SIZE, wy+PIXEL_SIZE/2)
        case(DIR_LEFT)
            v1 = vector2_type(wx+PIXEL_SIZE, wy) ! right top
            v2 = vector2_type(wx, wy+PIXEL_SIZE/2)
            v3 = vector2_type(wx+PIXEL_SIZE, wy+PIXEL_SIZE)
        case(DIR_DOWN)
            v1 = vector2_type(wx, wy) ! left top
            v2 = vector2_type(wx+PIXEL_SIZE/2, wy+PIXEL_SIZE)
            v3 = vector2_type(wx+PIXEL_SIZE, wy)
        case(DIR_UP)
            v1 = vector2_type(wx+PIXEL_SIZE/2, wy) ! center top
            v2 = vector2_type(wx, wy+PIXEL_SIZE)
            v3 = vector2_type(wx+PIXEL_SIZE, wy+PIXEL_SIZE)
        end select
        call draw_triangle(v1, v2, v3, PALETTE(id_color))

        if (mod(dir,2)==0) then
            call draw_circle(wx+PIXEL_SIZE/3,wy+PIXEL_SIZE/2,real(PIXEL_SIZE)/8, BLACK)
            call draw_circle(wx+2*PIXEL_SIZE/3,wy+PIXEL_SIZE/2,real(PIXEL_SIZE)/8, BLACK)
        else
            call draw_circle(wx+PIXEL_SIZE/2,wy+PIXEL_SIZE/3,real(PIXEL_SIZE)/8, BLACK)
            call draw_circle(wx+PIXEL_SIZE/2,wy+2*PIXEL_SIZE/3,real(PIXEL_SIZE)/8, BLACK)
        end if
    end subroutine render_snake_head


    subroutine render_score(snakes)
        type(snake_t), intent(in) :: snakes(:)

        character(len=80) :: tbuf
        integer :: i, wx, wy, fsize, twidth, gap, id_color

        gap = 5
        wx = gap
        fsize = WINDOW_TOP_MARGIN*0.60
        wy = WINDOW_TOP_MARGIN/2-fsize/2
        do i=1, size(snakes)
            write(tbuf,'(i0)') snakes(i)%length
            twidth = measure_text(' '//trim(tbuf)//' '//c_null_char, fsize)
            id_color = mod(snakes(i)%id-1, size(PALETTE))+1
            if (.not. snakes(i)%is_alive) then
                call draw_rectangle_lines_ex( &
                &   rectangle_type(real(wx-gap),real(wy-gap),real(twidth+2*gap),real(fsize+2*gap)), &
                &   3.0, PALETTE(id_color))
            end if
            call draw_text(' '//trim(tbuf)//' '//c_null_char, wx, wy, fsize, PALETTE(id_color))
            wx = wx + twidth + 3*gap
        end do
    end subroutine render_score


    subroutine grow_food(map)
        integer, intent(inout) :: map(:,:)
        real :: f(2)
        integer :: x, y, i
        integer, parameter :: SAFE_MAX=10000

        do i=1, SAFE_MAX
            call random_number(f)
            x = int(f(1)*MAP_WIDTH)+1
            y = int(f(2)*MAP_HEIGHT)+1
            if (map(x,y)==ID_FREE) then
                map(x,y) = ID_FOOD
                exit
            end if
        end do
        if (i==SAFE_MAX+1) print *, 'Warning: could not generate food'
    end subroutine grow_food


    function is_time_to_update() result(is)
      logical :: is
      real(c_double), save :: lasttime = 0.0_c_double
      real(c_double) :: nowtime

      nowtime = get_time()
      if (nowtime > lasttime+UPDATE_TSTEP) then
        is = .true.
        !lasttime = lasttime+UPDATE_TSTEP
        lasttime = nowtime
      else
        is = .false.
      end if
    end function


    ! ===========
    ! SNAKE CLASS
    ! ===========
    subroutine new_snake(this, snake_counter, map)
        type(snake_t), intent(out) :: this
        integer, intent(inout) :: snake_counter
        integer, intent(inout) :: map(:,:)

        real :: x(2)
        integer :: i
        integer, parameter :: SAFE_REPEAT = 10000

        allocate(this%head)
        do i=1,SAFE_REPEAT
            call random_number(x)
            this%head%x = int(x*shape(map))+1
            if (map(this%head%x(1),this%head%x(2))==ID_FREE) exit
        end do
        if (i==SAFE_REPEAT+1) then
            deallocate(this%head)
            print *, 'could not find free pixesl for a new snake'
            return
        end if

!leak_check_counter = leak_check_counter+1
        this%tail => this%head
        call random_number(x(1))
        this%dir = int(x(1)*4)+1
        this%is_growing = .false.
        this%is_alive = .true.
        this%ai_agent = 1 ! on default controlled by AI
        this%length = 1
        snake_counter = snake_counter+1
        this%id = snake_counter
        map(this%head%x(1),this%head%x(2)) = this%id
    end subroutine new_snake

    elemental subroutine free_snake(this)
        type(snake_t), intent(inout) :: this
        type(snakeslice_t), pointer :: next_to_free, to_free
        integer :: counter

        counter = 0
        if (associated(this%head) .or. associated(this%tail)) then
            next_to_free => this%tail
            do
                to_free => next_to_free
                if (.not. associated(to_free)) exit
                next_to_free => to_free%prev
                deallocate(to_free)
!leak_check_counter = leak_check_counter - 1
                counter = counter + 1
            end do
        end if
    end subroutine

    subroutine grow_snake(this)
        class(snake_t), intent(inout) :: this
        type(snakeslice_t), pointer :: newslice

        if (.not. this%is_alive) return

        ! will run only if not on cool-down, cool-down removed in update_snake
        if (.not. this%is_growing) then
            allocate(newslice)
!leak_check_counter = leak_check_counter + 1
            newslice%x = this%tail%x
            newslice%prev => this%tail
            this%tail => newslice
            this%is_growing = .true.
            this%length = this%length + 1
        else
            print *, 'Warning: cool-down is active'
        end if
    end subroutine grow_snake

    subroutine move_snake(this, map, collision)
        class(snake_t), intent(inout) :: this
        integer, intent(inout) :: map(:,:)
        integer, intent(out) :: collision
        type(snakeslice_t), pointer :: slice
        integer :: v(2)

        if (.not. this%is_alive) return

        ! un-mark pixel occupied by tail, except if a newly growed
        ! slice occupies same pixel as the old tail
        map(this%tail%x(1),this%tail%x(2)) = ID_FREE
        if (associated(this%tail%prev)) then
            if (all(this%tail%prev%x == this%tail%x)) &
                & map(this%tail%x(1),this%tail%x(2)) = this%id
        end if

        ! move all slices except head
        slice => this%tail
        do
          if (.not. associated(slice%prev)) exit
          slice%x = slice%prev%x
          slice => slice%prev
        end do

        ! move head and mark the pixel
        v = DIRS(:,this%dir)
        this%head%x(1) = modulo(this%head%x(1) + v(1) - 1, MAP_WIDTH) + 1
        this%head%x(2) = modulo(this%head%x(2) + v(2) - 1, MAP_HEIGHT) + 1
        collision = map(this%head%x(1),this%head%x(2))
        map(this%head%x(1),this%head%x(2)) = this%id
    end subroutine move_snake

    subroutine mancontrol_snake(this)
        class(snake_t), intent(inout) :: this

        if (is_key_down(KEY_D) .and. this%dir/=DIR_LEFT) then
            this%dir = DIR_RIGHT
        else if (is_key_down(KEY_A) .and. this%dir/=DIR_RIGHT) then
            this%dir = DIR_LEFT
        end if
        if (is_key_down(KEY_S) .and. this%dir/=DIR_UP) then
            this%dir = DIR_DOWN
        else if (is_key_down(KEY_W) .and. this%dir/=DIR_DOWN) then
            this%dir = DIR_UP
        end if

        if (is_key_pressed(KEY_X)) call grow_snake(this)
    end subroutine mancontrol_snake

    subroutine aicontrol_snake(this, game)
        class(snake_t), intent(inout) :: this
        type(game_t), intent(in) :: game

        integer :: repmin, repulse(4)
        integer, allocatable :: adir(:)
        real :: x

        if (.not. this%is_alive .or. this%ai_agent/=1) return

        ! pick the best direction, select randomly if more than one direction
        ! look the same
        repulse = look_at_dir(this, game, [1,2,3,4])
        repmin = minval(repulse)
        adir = pack([1,2,3,4], repulse==repmin)
        call random_number(x)
        this%dir = adir(1 + int(x*size(adir)))
        !if (repmin == AI_SIGHT_RANGE-1) print '("AI - cul de sac reached (",i0,")")', this%id
    end subroutine aicontrol_snake

    elemental function look_at_dir(snake, game, dir) result(repulse)
        type(snake_t), intent(in) :: snake
        type(game_t), intent(in) :: game
        integer, intent(in) :: dir
        integer :: repulse
!
! A ray from head in direction "dir" until snake or food is found
!              repulse value
! ----------------------------------
! H not allow  AI_SIGHT_RANGE
! H S          5 == AI_SIGHT_RANGE-1 (snake next to head)
! H > S        4
! H > > S      3
! H > > > > >  0 (nothing seen within AI_SIGHT_RANGE from head)
! H > > F      -3
! H > F        -4
! H F          -5 (food next to head)
!
        block ! LEFT <> RIGHT and UP <> DOWN jumps are not allowed
            integer :: d1, d2
            d1 = min(snake%dir, dir)
            d2 = max(snake%dir, dir)
            if (d2-d1 == 2) then ! change to this direction not allowed
                repulse = AI_SIGHT_RANGE
                return
            end if
        end block

        block ! collision with
            integer :: x, y, i
            x = snake%head%x(1)
            y = snake%head%x(2)
            do i=1, AI_SIGHT_RANGE
                x = modulo(x + DIRS(1,dir) - 1, MAP_WIDTH) + 1
                y = modulo(y + DIRS(2,dir) - 1, MAP_HEIGHT) + 1
                if (game%map(x,y) > ID_FREE) then ! seeing snake
                    repulse = AI_SIGHT_RANGE - i
                    return
                else if (game%map(x,y) == ID_FOOD) then ! seeing food
                    repulse = i - AI_SIGHT_RANGE
                    return
                end if
            end do
            repulse = 0 ! nothing have been seen
        end block
    end function look_at_dir

end module snake_mod



program main
    use snake_mod, only : c_null_char, WINDOW_WIDTH, WINDOW_HEIGHT, main_loop, &
    & TARGET_FPS, initialize, snake_t, game_t, NUMBER_OF_SNAKES !, leak_check_counter
    use raylib, only : window_should_close, init_window, close_window, set_target_fps
    implicit none (type, external)

    block
        type(snake_t) :: snakes(NUMBER_OF_SNAKES)
        type(game_t) :: game
        call init_window(WINDOW_WIDTH, WINDOW_HEIGHT, "Snake v 01"//c_null_char)
        call set_target_fps(TARGET_FPS)
        call initialize(snakes, game)
        do while (.not. window_should_close())
            call main_loop(snakes, game)
        end do
        call close_window()
    end block
!print *, 'Leak check = ',leak_check_counter
end program main

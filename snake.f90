module snake_mod
    use raylib
    use iso_c_binding, only : c_null_char
    use iso_fortran_env, only : int64
    implicit none (type, external)

!integer :: leak_check_counter = 0

    integer, parameter :: WINDOW_WIDTH=800, WINDOW_HEIGHT=600, PIXEL_SIZE=10
    integer, parameter :: MAP_WIDTH=WINDOW_WIDTH/PIXEL_SIZE, MAP_HEIGHT=WINDOW_HEIGHT/PIXEL_SIZE
    integer, parameter :: TARGET_FPS=60, UPDATE_FREQ=1

    integer, parameter :: AI_SIGHT_RANGE = MAP_WIDTH/1

    type(color_type), parameter :: PALETTE(*) = [ &
    & GRAY, DARKGRAY, YELLOW, GOLD, ORANGE, MAROON, GREEN, LIME, DARKGREEN, SKYBLUE, BLUE]

    integer(int64) :: frame_counter

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
        logical :: cool_down = .false.
        integer :: length = 1
        integer :: id
    contains
        final :: free_snake
    end type
    
contains

    subroutine initialize(snake, game)
        type(snake_t), intent(inout) :: snake
        type(game_t), intent(inout) :: game
        integer :: tmp

        game%state = STATE_GAME
        game%map = ID_FREE
        game%snake_counter = 0
        call new_snake(snake, game%snake_counter)
        call update_snake(snake, game%map, tmp)
        do tmp=1, 20
            call grow_food(game%map)
        end do
    end subroutine initialize


    subroutine main_loop(snake, game)
        type(snake_t), intent(inout) :: snake
        type(game_t), intent(inout) :: game
        integer :: collision
        character(len=32) :: buffer

        select case(game%state)
        case(STATE_GAME)
            call control_snake(snake)
            frame_counter = frame_counter + 1
            collision = ID_FREE
            if (mod(frame_counter,UPDATE_FREQ)==0) then
                call ai_snake(snake, game%map)
                call update_snake(snake, game%map, collision)
            end if
            if (collision > ID_FREE) then
                game%state = STATE_END
            else if (collision==ID_FOOD) then
                call grow_snake(snake)
                call grow_food(game%map)
            end if
        case(STATE_END)
            if (is_key_pressed(KEY_R)) call initialize(snake, game)
        case default
            error stop 'invalid state'
        end select

        write(buffer,"('length = ',i0)") snake%length
        if (game%state==STATE_END) buffer = trim(buffer)//' (press R to restart)'

        call begin_drawing()
            call clear_background(RAYWHITE)
            call render_map(game%map)
            call draw_text(trim(buffer)//c_null_char, 0, 0, 15, RED)
        call end_drawing()
    end subroutine main_loop


    subroutine render_map(map)
        integer, intent(in) :: map(:,:)
        integer :: x, y, wx, wy, id_color
        do x=1, MAP_WIDTH
            do y=1,MAP_HEIGHT
                wx = (x-1)*PIXEL_SIZE
                wy = (y-1)*PIXEL_SIZE
                select case(map(x,y))
                case default
                    if (map(x,y)<=0) error stop 'invalid value in map'
                    id_color = mod(map(x,y)-1, size(PALETTE))+1
                    call draw_rectangle(wx, wy, PIXEL_SIZE, PIXEL_SIZE, PALETTE(id_color))
                case (ID_FOOD)
                    call draw_circle(int(wx+0.5*PIXEL_SIZE), int(wy+0.5*PIXEL_SIZE), 0.5*real(PIXEL_SIZE), GREEN) 

                case (ID_FREE)
                end select
            end do
        end do
    end subroutine render_map


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


    ! ===========
    ! SNAKE CLASS
    ! ===========
    subroutine new_snake(this, snake_counter)
        type(snake_t), intent(out) :: this
        integer, intent(inout) :: snake_counter
        allocate(this%head)
!leak_check_counter = leak_check_counter+1
        this%tail => this%head
        this%head%x = [1,1]
        !this%v = 0
        this%dir = 3
        this%cool_down = .false.
        this%length = 1
        snake_counter = snake_counter+1
        this%id = snake_counter
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

        ! will run only if not on cool-down, cool-down removed in update_snake
        if (.not. this%cool_down) then
            allocate(newslice)
!leak_check_counter = leak_check_counter + 1
            newslice%x = this%tail%x
            newslice%prev => this%tail
            this%tail => newslice
            this%cool_down = .true.
            this%length = this%length + 1
        else
            print *, 'Warning: cool-down is active'
        end if
    end subroutine grow_snake

    subroutine update_snake(this, map, collision)
        class(snake_t), intent(inout) :: this
        integer, intent(inout) :: map(:,:)
        integer, intent(out) :: collision
        type(snakeslice_t), pointer :: slice
        integer :: v(2)

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

        this % cool_down = .false.
    end subroutine

    subroutine control_snake(this)
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
    end subroutine control_snake

    subroutine ai_snake(this, map)
        class(snake_t), intent(inout) :: this
        integer, intent(in) :: map(:,:)

        integer :: dir(4), nrep, idir, repmin, repnow, isel
        real :: x

        nrep = 0
        repmin = AI_SIGHT_RANGE
        do idir=1,4
            repnow = look_at_dir(this, map, idir)
            if (repnow < repmin) then
                nrep = 1
                repmin = repnow
                dir(nrep) = idir
            else if (repnow == repmin) then
                nrep = nrep + 1
                dir(nrep) = idir
            end if
        end do
        call random_number(x)
        if (repmin == AI_SIGHT_RANGE-1) then
            print *, 'AI - cul de sac reached'
        end if
        if (nrep==0) then
            print *, 'AI - could not do anything'
            return
        end if
        isel = 1 + int(x*nrep)
        this%dir = dir(isel)
    end subroutine ai_snake

    function look_at_dir(snake, map, dir) result(repulse)
        type(snake_t), intent(in) :: snake
        integer, intent(in) :: map(:,:)
        integer, intent(in) :: dir
        integer :: repulse
        
        block ! LEFT | RIGHT and UP | DOWN are not allowed
            integer :: d1, d2
            d1 = min(snake%dir, dir)
            d2 = max(snake%dir, dir)
            if (d2-d1 == 2) then
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
                if (map(x,y) > ID_FREE) then
                    repulse = AI_SIGHT_RANGE - i
                    return
                else if (map(x,y) == ID_FOOD) then
                    repulse = i - AI_SIGHT_RANGE
                    return
                end if
            end do
            repulse = 0
        end block
! x 1 2 3 4 5 6=SIGHT
! x S         6-1 = 5
! x . S       6-2 = 4
! x . . S     6-3 = 3
! x . . . . . 6-6 = 0        
! x . . F     3-6 = -3
! x . F .     2-6 = -4    
! x F . .     1-6 = -5
    end function look_at_dir

end module snake_mod



program main
    use snake_mod, only : c_null_char, WINDOW_WIDTH, WINDOW_HEIGHT, main_loop, &
    & TARGET_FPS, initialize, snake_t, game_t !, leak_check_counter
    use raylib, only : window_should_close, init_window, close_window, set_target_fps
    implicit none (type, external)

    block
        type(snake_t) :: snake
        type(game_t) :: game
        call init_window(WINDOW_WIDTH, WINDOW_HEIGHT, "Snake v 01"//c_null_char)
        call set_target_fps(TARGET_FPS)
        call initialize(snake, game)
        do while (.not. window_should_close())
            call main_loop(snake, game)
        end do
        call close_window()
    end block
!print *, 'Leak check = ',leak_check_counter
end program main
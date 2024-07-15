#!/bin/sh

set -xe

libs="-lraylib -lGL -lm -lpthread -ldl -lrt -lwayland-client -lwayland-cursor -lwayland-egl -lxkbcommon"

bindsrc=../fortran-raylib

bindobs="${bindsrc}/raylib_camera.o \
         ${bindsrc}/raylib.o \
         ${bindsrc}/raylib_math.o \
         ${bindsrc}/raylib_util.o"

flags="-fno-range-check --max-errors=1 -Wall -Wextra -pedantic -std=f2018"

gfortran $flags -J$bindsrc snake.f90 $bindobs $libs

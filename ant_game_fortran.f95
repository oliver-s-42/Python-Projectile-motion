!module for changing direction
module directions_change
contains
!subroutine changing the direction of ant and turns it 90 degrees clockwise
subroutine right_turn(pointer)
implicit none
integer, intent(inout) :: pointer
  select case (pointer)
  case (1) !North to east
    pointer=2
  case (2)! east to south
    pointer=3
  case (3) !south to west
    pointer=4
  case (4) !west to north
    pointer=1
  end select
end subroutine right_turn

!subroutine changing the direction of ant and turns it 90 degrees anticlockwise
subroutine left_turn(pointer)
implicit none
integer, intent(inout) :: pointer
  select case(pointer)
  case (1) !North to west
    pointer=4
  case (2)! east to north
    pointer=1
  case (3) !south to east
    pointer=2
  case (4) !west to south
    pointer=3
  end select
end subroutine left_turn
end module directions_change


program ant_game
use directions_change !calls module 
implicit none 
integer, parameter :: dp=selected_real_kind(15,300) !double precision
character (len=200) :: error_msg
integer :: istat, counter=0 , unit_out_ant=10,&
&  direction = 1!assumes north start

!grid initialisation
integer, dimension(-25:25, -25:25) :: grid !colour definer for each square
integer, dimension(2) :: position !current positiom marker: x and y co-ordinate
grid(:,:)=0 !starts all squares as white = 0
! do i = -25, 24, 2
!   grid(i,i)=1
!   end do !for varying the colour for grid initialisation
position(:)=0 !starts the ant at the origin

open(unit=unit_out_ant, file="final_results_ant.csv", action="readwrite", &
& iostat=istat, iomsg = error_msg) !opens the file and gives errors if fails
if (istat/=0) stop 'Error in opening file'

!whilst inside the grid, the loop executes distances and checks square colour
do while((abs(position(1))<25) .or. (abs(position(2))<25)) 
  counter = counter + 1 !counts number of loops 
  select case(grid(int(position(1)), int(position(2))))!choose case for position
   case (0) !if the square is white then
    call left_turn(direction) !turns left
    grid(int(position(1)), int(position(2)))=1 !sets current square to black
    call movement(position, direction) !moves forwards
   case (1) !if the square is black then 
    call right_turn(direction) !turns right
    grid(int(position(1)), int(position(2)))=0 !sets current square to white
    call movement(position, direction) !moves forwards
   end select
   write(unit_out_ant,*) position(1), ",", position(2), ",",counter &
   &, ",",mod_dist(position(1), position(2)) 
   !writes out co-ordinates, loop number and displacemnt from origin
end do   

close(unit_out_ant, iostat=istat, iomsg = error_msg) !closes file

contains
!function to find absolute distance from origin
function mod_dist(x,y)
  implicit none
  integer, intent(in) :: x,y
  real(kind=dp) :: mod_dist
  mod_dist= sqrt((real(x)**2)+(real(y)**2))
end function mod_dist

!subroutine to move ant position depending on direction
subroutine movement (position, direction)
integer, intent(in) :: direction !protects the direction from being changed
integer, dimension(2), intent(inout) :: position !position is what is changed
  select case(direction)
  case (1) !North
   position(2) = position(2) + 1
  case (2) !east
   position(1) = position(1) + 1
  case (3) !south
   position(2) = position(2) - 1
  case (4) !west
   position(1) = position(1) - 1
  case default
   print*, "big error" !case default in case of error
  end select
end subroutine movement

end program ant_game

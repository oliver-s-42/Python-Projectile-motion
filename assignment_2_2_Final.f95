program pi
implicit none
integer, parameter :: dp=selected_real_kind(15,300) !double precision
character (len=200) :: error_msg
integer :: unit_out_pi=10,  N=0,  total_w=0, istat
real(kind=dp) :: x_dp, y_dp, r_dp, pi_calc_dp, u_dp
open(unit=unit_out_pi, file="final_results_pi.csv", action="readwrite", &
& iostat=istat, iomsg = error_msg) !opens the file and gives errors if fails
if (istat/=0) stop 'Error in opening file'
do  
  N=N+1 !do loop for how many times it has occured
  call random_number(u_dp) !random number between 0 and 1
  x_dp=(2*u_dp)-1 !converts to random number between 1 and -1 
  call random_number(u_dp) 
  y_dp=(2*u_dp)-1
  r_dp=((x_dp**2)+(y_dp**2))**0.5 !calculates radius to circle 
  if (r_dp < 1) total_w=1+total_w !satisfies conditions 
  pi_calc_dp= 4.0_dp*total_w/N !caclulates value of pi
  if (N==1601) exit !exits at the 1600th loop
  write(unit_out_pi,*) N, ",", pi_calc_dp !writes out data into CSV file
end do
close(unit=unit_out_pi, ostat=istat, iomsg = error_msg) !closes file
end program
  
  


module write_history_module
  implicit none
  integer, parameter :: dp13 = selected_real_kind(15, 307)

contains
  subroutine write_velocity_history(t, dt, np, vp, xp)
    integer, intent(in) :: t, np
    real(dp13), intent(in) :: dt, vp(2, np), xp(2, np)
    real(dp13) :: v_avg, v_mag, x_avg, y_avg
    integer :: i

    v_avg = 0.0d0
    do i = 1, np
       v_mag = sqrt(vp(1,i)**2 + vp(2,i)**2) 
       v_avg = v_avg + v_mag
       x_avg = x_avg + xp(1,i)
       y_avg = y_avg + xp(2,i)
    end do
    v_avg = v_avg / np 
    x_avg = x_avg / np 
    x_avg = x_avg / np   

    open(unit=20, file="virt_history.dat", status="unknown", position="append")
    write(20,'(F12.6,3(1X,E20.12))') t*dt, v_avg, x_avg, y_avg
    close(20)
  end subroutine write_velocity_history
end module write_history_module
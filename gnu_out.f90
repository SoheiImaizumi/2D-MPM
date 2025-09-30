module gnuplot_write

  implicit none 
  integer, parameter :: dp12 = selected_real_kind(15, 307)

contains
  subroutine write_gnu(t, np, xp_new)
   
    integer :: np, i, t
    real(dp12) :: xp_new(2, np)
    character(len=256) :: filename

    write(filename,'("points_",i5.5,".dat")') t
    open(unit=10, file=filename, status="replace", action="write")

    do i = 1, np
      write(10,'(F15.8, 1x, F15.8)') xp_new(1, i), xp_new(2, i)
    end do

    close(10)

  end subroutine write_gnu
end module gnuplot_write
module write_vtk_module

  implicit none
  integer, parameter :: dp11 = selected_real_kind(15, 307)

contains
  subroutine vtk_write(t, np, xp, vp, sigma, volp)

    integer, intent(in) :: np, t
    real(dp11), intent(in) :: xp(2, np), vp(2, np), sigma(3, np), volp(np)
    character(len= 256) :: filename
    integer :: i

    write(filename,'("particles_",i5.5,".vtk")') t
    open(unit=10, file=filename, status="replace", action="write")

    write(10,'(A)') "# vtk DataFile Version 3.0"
    write(10,'(A)') "Particle data"
    write(10,'(A)') "ASCII"
    write(10,'(A)') "DATASET POLYDATA"

    write(10,'("POINTS ",i0," float")') np
    do i = 1, np
      write(10,'(3E15.6)') xp(1,i), xp(2,i), 0.0_dp11
    end do

    write(10,'("POINT_DATA ",i0)') np

    write(10,'(A)') "VECTORS velocity float"
    do i = 1, np
      write(10,'(3E15.6)') vp(1,i), vp(2,i), 0.0_dp11
    end do

    write(10,'(A)') "SCALARS volume float 1"
    write(10,'(A)') "LOOKUP_TABLE default"
    do i = 1, np
      write(10,'(E15.6)') volp(i) + 1.0e-12
    end do

    write(10,'(A)') "TENSORS stress float"
    do i = 1, np
      write(10,'(3E15.6)') sigma(1,i), sigma(3,i), 0.0_dp11   ! xx, xy
      write(10,'(3E15.6)') sigma(3,i), sigma(2,i), 0.0_dp11
      write(10,'(3E15.6)')  0.0_dp11, 0.0_dp11, 0.0_dp11
    end do

    close(10)

  end subroutine vtk_write

end module write_vtk_module
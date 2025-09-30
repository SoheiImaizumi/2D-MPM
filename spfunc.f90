module shape_function_module   !over all nodes

  implicit none
  integer, parameter :: dp4 = selected_real_kind(15, 307)

Contains
  subroutine shapefunc(xI, xp, NIp, grad_NIp, h, nI, np)

    integer :: i, l, j
    integer, intent(in) :: np, nI
    real(dp4) :: dx
    real(dp4), intent(in) :: xI(2, nI), xp(2, np), h
    real(dp4), intent(out) :: NIp(2, nI, np), grad_NIp(2, nI, np)

    NIp = 0.0d0
    grad_NIp = 0.0d0 

    do j = 1, 2
      do l = 1, nI
        do i = 1, np
          dx = xp(j,i) - xI(j,l)
          if (abs(dx) <= h) then
            NIp(j,l,i) = 1.0d0 - abs(dx)/h
            grad_NIp(j,l,i) = sign(-1.0d0, dx)/h   
          else
            NIp(j,l,i) = 0.0d0
            grad_NIp(j,l,i) = 0.0d0
          end if
        end do
      end do
    end do

  end subroutine shapefunc

end module shape_function_module
module p_renew_module  ! every single particle

  implicit none
  integer, parameter :: dp10 = selected_real_kind(15, 307)

Contains
  subroutine p_renew(nI, np, dt, E, nu, volp0, Fp, Fp_new, volp_new, grad_NIp, vI_dm, sigma, sigma_new)

  integer :: l, p, q, i, r, s
  integer, intent(in) :: nI, np
  real(dp10), intent(in) :: E, nu, dt, volp0(np), Fp(2, 2, np), grad_NIp(2, nI, np), vI_dm(2, nI), sigma(3, np)
  real(dp10), intent(out) :: Fp_new(2, 2, np), volp_new(np), sigma_new(3, np)
  real(dp10) :: lambda, mu, JJ
  real(dp10), allocatable :: II(:,:), sig_inc(:,:), Lp(:,:,:), symLp(:,:,:), C(:,:,:,:), &
                           sigma_tsr(:,:), sigma_new_tsr(:,:)

  allocate(II(2, 2), sig_inc(2, 2), sigma_tsr(2, 2), sigma_new_tsr(2, 2))
  allocate(Lp(2, 2, np), symLp(2, 2, np))
  allocate(C(2, 2, 2, 2))

  Lp = 0.0d0
  symLp = 0.0d0
  Fp_new = 0.0d0
  sig_inc = 0.0d0
  II = 0.0d0
  C = 0.0d0
  sigma_tsr = 0.0d0
  sigma_new_tsr = 0.0d0

  mu = E /(2*(1 + nu))
  lambda = E* nu/((1 + nu) * (1 - 2* nu))

  do p = 1, 2
    do q = 1, 2
      do r = 1, 2
        do s = 1, 2
          C(p, q, r, s) = lambda* kronecker_delta(p, q) * kronecker_delta(r, s) &
                          + mu* (kronecker_delta(p, r) * kronecker_delta(q, s) &
                          + kronecker_delta(p, s) * kronecker_delta(q, r))    
        end do
      end do
    end do
  end do

  do p = 1, 2
    II(p, p) = 1.0d0
  end do
  
  Lp = 0.0d0
  symLp = 0.0d0

  do i = 1, np
    JJ = 0.0d0
    sigma_new_tsr = 0.0d0

    sigma_tsr(1, 1) = sigma(1, i)
    sigma_tsr(2, 2) = sigma(2, i)
    sigma_tsr(1, 2) = sigma(3, i)
    sigma_tsr(2, 1) = sigma(3, i)

    do l = 1, nI
      do p = 1, 2
        do q = 1, 2
          Lp(p, q, i) = Lp(p, q, i) + grad_NIp(p, l, i) * vI_dm(q, l)
        end do
      end do
    end do

    Fp_new(:, :, i) = matmul((II + dt * Lp(:, :, i)), Fp(:, :, i))

    do p = 1, 2
      do q = 1, 2
        symLp(p, q, i) = (Lp(p, q, i) + Lp(q, p, i)) /2
      end do
    end do 

    sig_inc(:,:) = 0.0d0

    do p = 1, 2
      do q = 1, 2
        do r = 1, 2
          do s = 1, 2
            sig_inc(p, q) = sig_inc(p, q) + C(p, q, r, s) * symLp(r, s, i)
          end do
        end do
      end do
    end do

    JJ = abs((Fp_new(1, 1, i) * Fp_new(2, 2, i)) - (Fp_new(2, 1, i) * Fp_new(1, 2, i)))
    volp_new(i) = JJ * volp0(i)

    do p = 1, 2
      do q = 1, 2
        sigma_new_tsr(p, q) = sigma_tsr(p, q) + dt* sig_inc(p, q)

        if (sigma_new_tsr(p, q) <= 1.0e-15) then
          sigma_new_tsr(p, q) = 0.0d0
        end if
      end do
    end do
    
    sigma_new(1, i) = sigma_new_tsr(1, 1)
    sigma_new(2, i) = sigma_new_tsr(2, 2)
    sigma_new(3, i) = sigma_new_tsr(1, 2)
  end do

  deallocate(II, sig_inc, sigma_tsr, sigma_new_tsr, Lp, symLp, C)
  
  Contains
    pure real(dp10) function kronecker_delta(i, j) result(delta)
      implicit none
      integer, intent(in) :: i, j
      if (i == j) then
        delta = 1.0d0
      else
        delta = 0.0d0
       end if
    end function kronecker_delta  
  end subroutine p_renew
end module p_renew_module
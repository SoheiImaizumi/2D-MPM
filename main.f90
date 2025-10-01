program mpm2d_paraboric

  use initial_module
  use buildgrid_module
  use particle_distribution_module
  use shape_function_module
  use p2n_interpolation_module
  use I_renew_module
  use boundary_module
  use n2p_extrapolation_module
  use double_mapping_module
  use p_renew_module
  use gnuplot_write
  use write_history_module
  use write_vtk_module

  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)      

  integer :: t
  integer :: step, np, npm, nI, nI_horiz, nI_vert
  real(dp) :: M0, h, dt, vol, g, E, nu

  real(dp), parameter :: eps = 1.0e-15_dp

  real(dp), allocatable :: v0(:)            
  real(dp), allocatable :: mp(:), volp(:), volp0(:), volp_new(:)
  real(dp), allocatable :: xp(:,:), vp(:,:), pp(:,:)
  real(dp), allocatable :: vp_new(:,:), xp_new(:,:), pp_new(:,:)
  real(dp), allocatable :: sigma(:,:), sigma_new(:,:)
  real(dp), allocatable :: Fp(:,:,:), Fp_new(:,:,:)
  real(dp), allocatable :: mI(:), volI(:)
  real(dp), allocatable :: xI(:,:)       
  real(dp), allocatable :: pI(:,:), pI_new(:,:), fI(:,:), vI_dm(:,:)
  real(dp), allocatable :: NIp(:,:,:), grad_NIp(:,:,:)

  real(dp), allocatable :: fIint(:,:), fIext(:,:)

  allocate(v0(2))

  call init(M0, h, step, dt, vol, np, nI_horiz, nI_vert, v0, g, E, nu)
  call bgrid(xI, h, nI_horiz, nI_vert, nI)

  allocate(mp(np), volp(np), volp0(np), volp_new(np))
  allocate(mI(nI), volI(nI))
  allocate(xp(2, np), vp(2, np), pp(2, np), &
           vp_new(2, np), xp_new(2, np), pp_new(2, np))
  allocate(sigma(3, np), sigma_new(3, np))
  allocate(pI(2, nI), fI(2, nI), pI_new(2, nI), vI_dm(2, nI))
  allocate(NIp(2, nI, np), grad_NIp(2, nI, np))
  allocate(Fp(2, 2, np), Fp_new(2, 2, np))
  allocate(fIint(2, nI), fIext(2, nI))

  call particle_distribution(M0, h, np, npm, vol, v0, xp, pp, vp, mp, volp)

  do t = 1, step
    pI = 0.0d0
    fI = 0.0d0
    volI = 0.0d0
    mI = 0.0d0

    !write(*,*) 'momentum', pp(2, :)
    write(*,*) 'height', xp(2, :)
    
    call shapefunc(xI, xp, NIp, grad_NIp, h, nI, np)
    call p2n_interpolation(NIp, grad_NIp, mp, pp, volp, g, sigma, pI, fI, &
                           volI, mI, nI, np)
    call I_renew(nI, dt, pI, fI, pI_new)
    call bcondition(nI_horiz, fI, pI_new, nI)
    call n2p_extrapolation(nI, dt, xp, mI, vp_new, NIp, pp, &
                           pp_new, vp, xp_new, pI_new, np, npm, fI)
    call double_mapping(NIp, pp_new, mI, nI, np, vI_dm)
    call p_renew(nI, np, dt, E, nu, volp0, Fp, Fp_new, &
                 volp_new, grad_NIp, vI_dm, sigma, sigma_new)
    call write_gnu(t, np, xp_new)

    pp = pp_new
    vp = vp_new
    xp = xp_new
    sigma = sigma_new
    volp = volp_new
    Fp = Fp_new
    
    !call vtk_write(t, np, xp, vp, sigma, volp)
    call write_velocity_history(t, dt, np, vp, xp)

  end do

  deallocate(v0, volp0, mp, volp, volp_new)
  deallocate(xp, vp, pp, vp_new, xp_new, pp_new)
  deallocate(sigma, sigma_new, Fp, Fp_new)
  deallocate(mI, volI, pI, pI_new, fI, vI_dm)
  deallocate(NIp, grad_NIp, xI)
  deallocate(fIint, fIext)

end program mpm2d_paraboric
module initial_module

  implicit none
  integer, parameter :: dp1 = selected_real_kind(15, 307)

Contains
  subroutine init(M0, h, step, dt, vol, np, nI_horiz, nI_vert, v0, g, E, nu)

    integer :: np, nI_horiz, nI_vert, step
    real(dp1) :: M0, h, dt, vol, g, E, nu
    !real(dp1) :: CFL, c 
    real(dp1) :: v0(2)

    np = 16
    nI_horiz = 40
    nI_vert = 40
    step = 5000

    !c = 3.4d2
    !CFL = 5.0e-1

    M0 = 1.0e2
    h = 5.0e-2
    vol = 1.0e-2
    v0(1) = 0.0d0
    v0(2) = 0.0d0

    g = 1.0d2

    E = 1.0d2
    nu = 0.3
    
    dt = 0.0001

  end subroutine init
end module initial_module

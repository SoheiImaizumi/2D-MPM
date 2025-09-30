module initial_module

  implicit none
  integer, parameter :: dp1 = selected_real_kind(15, 307)

Contains
  subroutine init(M0, h, step, dt, vol, np, nI_horiz, nI_vert, v0, g, E, nu)

    integer :: np, nI_horiz, nI_vert, step
    real(dp1) :: M0, h, dt, vol, g, E, nu
    real(dp1) :: CFL, c 
    real(dp1) :: v0(2)

    np = 16
    nI_horiz = 40
    nI_vert = 40
    step = 1000

    c = 3.4d2
    CFL = 5.0e-1

    M0 = 2.0e-1
    h = 2.0e-2
    vol = 1.0e-4
    v0(1) = 5.0d-1
    v0(2) = 0.0d0

    g = 1.0d3

    E = 1.0e4
    nu = 3.0e-2
    
    dt = CFL * h / c

  end subroutine init
end module initial_module

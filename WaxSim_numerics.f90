module Numerics



!   Used modules
    use                 ::  Constants

!   All variables must be declared
    implicit none

!   Private subroutines and variables
    private

!   Module public subroutines
    public              ::  Hld_sol
    public              ::  Iblnr_1d
    public              ::  Iblnr_2d
    public              ::  Order_1d
    public              ::  Qgauss_la
    public              ::  Search_1d
    public              ::  Zbrent
    public              ::  Zmuller

!   Subroutines and functions interface
    abstract interface

!       Zfun interface for Zbrent and Zmuller subroutines
        function Zfun (x)
            import              ::  rp                  !   Real precision          [-]
            real(rp),intent(in) ::  x                   !   Input variable          [-]
            real(rp)            ::  Zfun
        end function

!       Qfun interface for Qgauss_la function
        pure function Qfun (x1, x2)
            import              ::  rp                  !   Real precision          [-]
            real(rp),intent(in) ::  x1                  !   Input variable          [-]
            real(rp),intent(in) ::  x2                  !   Input variable          [-]
            real(rp)            ::  Qfun
        end function
    end interface

!	Module subroutines and functions
    contains

! ***************************************************************************************

    pure function           Hld_sol     ( hlf )
    ! ***********************************************************************************


    !	Input variables
        real(rp),intent(in)         ::  hlf             !	Liq film holdup         [-]

    !	Output variables
        real(rp)                    ::  Hld_sol         !	Dim.less liquid level   [-]

    !	Internal variables
        real(rp)                    ::  hld             !	Dim.less liquid level   [-]

    ! ***********************************************************************************

    !	Dimensionless liquid level. 1st approximation
        hld = p25 * ((pi * (hlf - one) + acos(two * hlf - one)) / &
            sqrt(one - (two * hlf - one) ** 2) + two * hlf + one)

    !	Dimensionless liquid level. 2nd approximation
        hld = p25 * ((pi * (hlf - one) + acos(two * hld - one)) / &
            sqrt(one - (two * hld - one) ** 2) + two * hld + one)

    !	Dimensionless liquid level. 3rd approximation
        hld = p25 * ((pi * (hlf - one) + acos(two * hld - one)) / &
            sqrt(one - (two * hld - one) ** 2) + two * hld + one)

    !	Dimensionless liquid level. 4th approximation
            Hld_sol = p25 * ((pi * (hlf - one) + acos(two * hld - one)) / &
                sqrt(one - (two * hld - one) ** 2) + two * hld + one)

    ! ***********************************************************************************
    end function

! ***************************************************************************************

    pure subroutine         Iblnr_1d    ( x, i, f, xi, fi )
    ! ***********************************************************************************
    !                       * *  TULSA UNIVERSITY FLUID FLOW PROJECTS  * *
    ! **  SUBROUTINE:       Iblnr_1d
    ! **  VERSION:          2.0 Fortran 2003


    !	Input variables
        real(rp),intent(in)         ::  x(:)            !   x array                 [-]
        integer(ip),intent(in)      ::  i               !   x array location        [-]
        real(rp),intent(in)         ::  f(:)            !   f array data            [-]
        real(rp),intent(in)         ::  xi              !   xi coordinate point     [-]

    !   Output variables
        real(rp),intent(out)        ::  fi              !   Interpolation result    [-]

    !   Internal variables
        real(rp)                    ::  alpha           !   Weight coeficient       [-]

    ! ***********************************************************************************

    !   Upper or lower approximation
        alpha = (xi - x(i)) / (x(i+1) - x(i))

    !   fi approximation
        fi = f(i) * (one - alpha) + f(i+1) * alpha

    ! ***********************************************************************************
    end subroutine

! ***************************************************************************************

    pure subroutine         Iblnr_2d    ( alpha, beta, i, j, f, fi )


    !	Input variables
        real(rp),intent(in)         ::  alpha           !   Weight coeficient       [-]
        real(rp),intent(in)         ::  beta            !   Weight coeficient       [-]
        integer(ip),intent(in)      ::  i               !   x array location        [-]
        integer(ip),intent(in)      ::  j               !   y array location        [-]
        real(rp),intent(in)         ::  f(:,:)          !   f array data            [-]

    !   Output variables
        real(rp),intent(out)        ::  fi              !   Interpolation result    [-]

    ! ***********************************************************************************

    !   fi approximation
        fi = (f(i,j) * (one - alpha) + f(i+1,j) * alpha) * (one  - beta) + &
             (f(i,j+1) * (one - alpha) +  f(i+1,j+1) * alpha) * beta

    ! ***********************************************************************************
    end subroutine

! ***************************************************************************************

    pure function           Order_1d    ( x, nx )


    !	Input variables
        real(rp),intent(in)         ::  x(:)            !   Input array            [-]
        integer(ip), intent(in)     ::  nx              !   Input array lenght     [-]

    !   Output variables
        logical                     ::  Order_1d        !   Monotonic result        [t/f]

    !   Internal variables
        integer(ip)                 ::  i               !   Counter                 [-]

    ! ***********************************************************************************

    !   Monotonic test
        do i = 1, nx - 1
            if (x(i) > x(i+1)) then

                Order_1d = .false.
                return
            end if
        end do

    !   Solution
        Order_1d = .true.

    ! ***********************************************************************************
    end function

! ***************************************************************************************

    function                Qgauss_la    ( f, y )


    !	Input variables
        procedure(Qfun)             ::  f               !   External interface      [-]
        real(rp),intent(in)         ::  y               !   Auxiliary variable      [-]

    !   Output variables
        real(rp)                    ::  Qgauss_la       !   Integral estimate       [-]

    !  Internal variables
        integer(ip)                 ::  i               !   Counter                 [-]
        real(rp)                    ::  tmp(16)         !   Auxiliary vector        [-]
        real(rp),parameter          ::  x(16) = [ &
                                            0.0876494104789278403602_rp, &
                                            0.462696328915080831881_rp, &
                                            1.141057774831226856878_rp, &
                                            2.129283645098380616326_rp, &
                                            3.437086633893206645235_rp, &
                                            5.078018614549767912923_rp, &
                                            7.070338535048234130396_rp, &
                                            9.438314336391938783947_rp, &
                                            12.21422336886615873694_rp, &
                                            15.44152736878161707676_rp, &
                                            19.18015685675313485466_rp, &
                                            23.51590569399190853182_rp, &
                                            28.57872974288214036752_rp, &
                                            34.58339870228662581453_rp, &
                                            41.94045264768833263547_rp, &
                                            51.70116033954331836434_rp]
        real(rp), parameter         ::  w(16) = [ &
                                            0.22503631486424725191_rp, &
                                            0.5258360527623424536_rp, &
                                            0.83196139168708708767_rp, &
                                            1.1460992409637517046_rp, &
                                            1.4717513169668085887_rp, &
                                            1.8131346873813481649_rp, &
                                            2.1755175196946074521_rp, &
                                            2.5657627501650292092_rp, &
                                            2.99321508637137516518_rp, &
                                            3.47123448310209029087_rp, &
                                            4.0200440864446688665_rp, &
                                            4.67251660773285426737_rp, &
                                            5.48742065798615247121_rp, &
                                            6.58536123328921366083_rp, &
                                            8.2763579843642344865_rp, &
                                            11.82427755165843478669_rp]

    ! ***********************************************************************************

    !   GNU Fortran compiler
        forall (i =1:16) tmp(i) = f(x(i),y)

    !   Gauss-Laguerre quadrature for improper integrals
        Qgauss_la = dot_product(tmp, w)

    ! ***********************************************************************************
    end function

! ***************************************************************************************

    pure function           Search_1d   ( x, xi, nx )


    !	Input variables
        real(rp),intent(in)         ::  x(:)            !   Input array            [-]
        real(rp),intent(in)         ::  xi              !   Input value             [-]
        integer(ip), intent(in)     ::  nx              !   Input array lenght     [-]

    !   Output variables
        integer(ip)                 ::  Search_1d       !   Lower index closer to xi[-]

    !   Internal variables
        integer(ip)                 ::  l               !   Bisection lower index   [-]
        integer(ip)                 ::  m               !   Bisection medium index  [-]
        integer(ip)                 ::  u               !   Bisection upper index   [-]

    ! ***********************************************************************************

    !   xi between x(1) and x(nx)
        l = 1
        u = nx

    !   Bisection
        do while (u - l > 1)

    !       Mid point
            m = (l + u) / 2
            if (xi > x(m)) then

    !           Lower bound
                l = m
            else

    !           Upper bound
                u = m
            end if
        end do

    !   Solution
        Search_1d = l

    ! ***********************************************************************************
    end function

! ***************************************************************************************

    subroutine              Zbrent      ( f, a, b, inf )


    !	Input variables
        procedure(Zfun)             ::  f               !   External interface      [-]

    !	Input / Output variables
        real(rp),intent(inout)      ::  a               !   User boundary. Last 'f' [-]
        real(rp),intent(inout)      ::  b               !   User boundary. 'f' Root [-]

    !   Output variables
        type(inf_type),intent(out)  ::  inf             !   Report data-type
                                    !   inf%er              Error flag              [-]
                                    !   inf%it              Number of iterations    [-]
                                    !   inf%nm              Error name              [-]


    !   Internal variables
        integer(ip)                 ::  ic              !   Iteration counter       [-]
        real(rp)                    ::  c               !   Interval (a, b)         [-]
        real(rp)                    ::  d               !   Interval (a, b)         [-]
        real(rp)                    ::  e               !   Interval (b - a)        [-]
        real(rp)                    ::  fa              !   Function at 'a'         [-]
        real(rp)                    ::  fb              !   Function at 'b'         [-]
        real(rp)                    ::  fc              !   Function at 'c'         [-]
        real(rp)                    ::  p               !   Quadratic inter. weigth [-]
        real(rp)                    ::  q               !   Quadratic inter. weigth [-]
        real(rp)                    ::  r               !   Quadratic inter. weigth [-]
        real(rp)                    ::  xm              !   Interval mid point      [-]
        real(rp)                    ::  s               !   Lineal inter. weigth    [-]
        real(rp)                    ::  tol_int         !   Internal tolerance      [-]

    ! ***********************************************************************************

    !	Subroutine definitions
        fa = f(a)
        fb = f(b)
        fc = fb
        c = b
        e = b - a

    !	Test for f(a) & f(b) sign
        if (fa * fb > zero) then

    !       Report data-type
            inf = inf_type ( .true., 0, 'same_sgn' )

   !        Subroutine exit
            return
        end if

    !   Main loop
        do ic = 1, tol%max

    !       Selecting the side of bisection
            if (fb * fc > zero) then

                c = a
                fc = fa
                d = b - c
                e = d
            end if

    !       Flipping order
           if (abs(fc) < abs(fb)) then

                a = b
                b = c
                c = a
                fa = fb
                fb = fc
                fc = fa
            end if

    !       Iteration tolerance
            tol_int =  two * tol%abs * abs(b) + p5 * tol%rel

    !       Mid point
            xm = p5 * (c - b)

    !		Test for first convergence criteria and second convergence criteria
            if (abs(fb) <= tol%abs .or. abs(xm) <= tol_int) then

    !           Last computed root value
                a = fc

    !           Report data-type
                inf = inf_type (.false., ic, no_error)

    !           Subroutine exit
                return
            end if

    !		Bisection forced
            if (abs(e) >= tol_int .or. abs(fa) > abs(fb)) then

    !           Interpolation factor
                s = fb / fa

    !           Interval selection
                if (abs(a - c) < tol%abs) then

    !				Secant method
                    p = two * s * xm
                    q = one - s
                else

    !				Inverse quadratic interpolation
                    q = fa / fc
                    r = fb / fc
                    p = s * (two * xm * q * (q - r) - (b - a) * (r - one))
                    q = (q - one) * (r - one) * (s - one)
                end if

    !           Check whether in bounds
                if (p > zero) then

                    q = -q
                else

                    p = -p
                end if

    !			If (2.*p < min(3.*xm*q-abs(tol*q), abs(e*q))) then force bisection
                if (two * p < min(3._rp * xm * q - abs(tol_int * q), abs(e * q))) then

                    e = d
                    d = p / q
                else

                    d = xm
                    e = d
                end if
            else

    !			Bisection
                d = xm
                e = d
            end if

    !       New trial root
            a = b
            fa = fb
            if (abs(d) > tol_int) then

                b = b + d
            else

                b = b + sign(tol_int, xm)
            end if
            fb = f(b)
        end do

    !   Last computed root value
        a = fc

    !   Report data-type
        inf = inf_type ( .true., ic, 'max_eval' )

    ! ***********************************************************************************
    end subroutine

! ***************************************************************************************

    subroutine              Zmuller     ( f, x, nr, inf )


    !	Input variables
        procedure(Zfun)             ::  f               !   External interface      [-]
        integer(ip),intent(in)      ::  nr              !   Roots to be solve       [-]

    !	Input / Output variables
        real(rp),intent(inout)      ::  x(nr)           !   Initial guess and output[-]

    !   Output variables
        type(inf_type),intent(out)  ::  inf(nr)         !   Report data-type
                                    !   inf%er              Error flag              [-]
                                    !   inf%it              Number of iterations    [-]
                                    !   inf%nm              Error name              [-]

    !   Internal variables
        integer(ip)                 ::  ic              !   Iterations per root     [-]
        integer(ip)                 ::  j               !   Root number             [-]
        integer(ip)                 ::  k               !   Counter                 [-]
        real(rp)                    ::  a               !   Point 'a'               [-]
        real(rp)                    ::  b               !   Point 'b'               [-]
        real(rp)                    ::  c               !   Point 'c'               [-]
        real(rp)                    ::  d               !   Point 'd'               [-]
        real(rp)                    ::  bi              !   'b' sec ord eq          [-]
        real(rp)                    ::  di              !   Non scale corection step[-]
        real(rp)                    ::  dii             !   Non scale corection step[-]
        real(rp)                    ::  den             !   'b^2-4*a*c' sec ord eq  [-]
        real(rp)                    ::  fa              !   Function at 'a'         [-]
        real(rp)                    ::  fb              !   Function at 'b'         [-]
        real(rp)                    ::  fc              !   Function at 'c'         [-]
        real(rp)                    ::  fd              !   Function at 'd'         [-]
        real(rp)                    ::  h               !   Correction step         [-]
        real(rp)                    ::  tol_int         !   Internal tolerance      [-]

    ! ***********************************************************************************

    !   Loop per root
        root_loop: do j = 1, nr

    !       Setup per root
            a = x(j) * 0.9_rp
            b = x(j) * 1.1_rp
            c = x(j)
            d = a

    !       Iteration loop
            do ic = 1, tol%max

    !           Holdup boundaries
                if (d < tol%eps) then

    !               CME solution for holdup lower boundary
                    fd = -one / tol%eps
                else if (d > one - tol%eps) then

    !               CME solution for holdup upper boundary
                    fd = one / tol%eps
                else

    !               CME solution for holdup
                    fd = f(d)
                end if

    !           Roots proximity
                do k = 2, j

    !               Derivaty diference
                    if (abs(d - x(k-1)) < tol%eps) then

    !                   Increase diference
                        d = d + tol%eta

    !                   Cycle to next root proximity
                        cycle
                    end if

    !               Function correction
                    fd = fd / (d - x(k-1))
                end do

    !           Muller method setup
                select case (ic)
    !               ---------------------------------------------------------------------
                    case (1)

    !                   Firts evaluation
                        d = b
                        fa = fd
    !               ---------------------------------------------------------------------
                    case (2)

    !                   Second evaluation
                        d = c
                        fb = fd
    !               ---------------------------------------------------------------------
                    case (3)

    !                   Third evaluation
                        fc = fd
                        h = -p1 * x(j)
                        bi = p25 * (fa - fb)
                        den = bi ** 2 + fc * (fc - p5 * (fb + fa))
                        if (den > zero) then

                            den = bi + sign(sqrt(den), bi)
                        else

                            den = bi + dmach(2)
                        end if

    !                   New step
                        di = -fc / den
                        h = di * h
                        d = d + h
    !               ---------------------------------------------------------------------
                    case default

                        if (abs(fd) < abs(10._rp * fc)) then

    !                       Default evaluation
                            fa = fb
                            fb = fc
                            fc = fd
                            dii = one + di
                            bi = fa * di ** 2 - fb * dii ** 2 + fc * (di + dii)
                            den = bi ** 2 - four * fc * di * dii * (fa * di - fb * dii + fc)

    !                       Denominator verification
                            if (den > zero) then

                                den = bi + sign(sqrt(den), bi)
                            else

                                den = bi + dmach(2)
                            end if

    !                       New step
                            di = -two * dii * fc / den
                            h = di * h
                            d = d + h

    !                       Iteration tolerance
                            tol_int = two * tol%abs * abs(d) + p5 * tol%eps

    !                       Test for convergence
                            if (abs(fd) <= tol%abs .or. abs(p5 * h) <= tol_int) then

    !                           Final root
                                x(j) = d

                                if (d > zero .and. d < one) then

    !                               Numerical report
                                    inf(j) = inf_type ( .false., ic, no_error )
                                else

    !                               Numerical report
                                    inf(j) = inf_type ( .true., ic, 'boundary' )
                                end if

    !                           Cycle to next root
                                cycle root_loop
                            end if
                        else

    !                       Step decrease
                            di = p5 * di
                            h = p5 * h
                            d = d - h
                        end if
    !               ---------------------------------------------------------------------
                end select
            end do

    !	    Maximum function evaluations achieved
            if (ic > tol%max) then

    !           Solution
                x(j) = zero

    !           Report data-type
                inf(j) = inf_type ( .true., ic, 'max_eval' )
            end if

        end do root_loop

    ! ***********************************************************************************
    end subroutine

! ***************************************************************************************
end module

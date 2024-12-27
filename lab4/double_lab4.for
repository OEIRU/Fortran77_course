PROGRAM main
    IMPLICIT NONE
    REAL*8 h, a, b
    INTEGER N, max, i
    COMMON /gridparam/ h, a, b, N
    REAL*8 grid(100000000)

    PRINT *, '1) trapezia'
    PRINT *, '3) gauss-4'
    PRINT *, '4) vihod'
    
    READ *, i
    IF (i .EQ. 4) GOTO 100

    PRINT *, 'Enter segment and [a,b]'
    READ *, max, a, b

    N = 2
    GOTO (10, 30) i
 10 CALL trapeze(grid, max)
    GOTO 100
 30 CALL gauss4(grid, max)
    GOTO 100

100 END

    SUBROUTINE Gridmake(grid)
    IMPLICIT NONE
    REAL*8 h, a, b
    INTEGER N, i
    COMMON /gridparam/ h, a, b, N
    REAL*8 grid(N)

    h = (b - a) / (N - 1)
    grid(1) = a
    DO i = 2, N
        grid(i) = a + h * (i - 1)
    END DO
    END

    SUBROUTINE trapeze(grid, max)
    IMPLICIT NONE
    REAL*8 h, a, b
    INTEGER N, max, i
    COMMON /gridparam/ h, a, b, N
    REAL*8 grid(N)
    REAL*8 ans, fun
    EXTERNAL fun

    DO WHILE (N .LE. max + 1)
        CALL Gridmake(grid)
        ans = h * (fun(grid(1)) + fun(grid(N))) / 2.0
        DO i = 2, N - 1
            ans = ans + h * fun(grid(i))
        END DO
        PRINT *, 'Trapezoidal rule result for N =', N, ':', ans
        N = 2 * N - 1
    END DO
    END

    SUBROUTINE gauss4(grid, max)
    IMPLICIT NONE
    REAL*8 h, a, b
    INTEGER N, max, i
    COMMON /gridparam/ h, a, b, N
    REAL*8 grid(N)
    REAL*8 ans, x1, x2, x3, x4, w1, w2, w3, w4, mid, fun
    EXTERNAL fun

    w1 = 0.3478548451
    w2 = 0.6521451549
    w3 = 0.6521451549
    w4 = 0.3478548451
    x1 = -0.8611363116
    x2 = -0.3399810436
    x3 = 0.3399810436
    x4 = 0.8611363116

    DO WHILE (N .LE. max + 1)
        CALL Gridmake(grid)
        ans = 0.0
        DO i = 1, N - 1
            h = (grid(i + 1) - grid(i)) / 2.0
            mid = (grid(i + 1) + grid(i)) / 2.0
            ans = ans + h * (w1 * fun(mid + h * x1) + 
   &                         w2 * fun(mid + h * x2) +
   &                         w3 * fun(mid + h * x3) + 
   &                         w4 * fun(mid + h * x4))
        END DO
        PRINT *, 'Gauss-4 rule result for N =', N, ':', ans
        N = 2 * N - 1
    END DO
    END

    REAL*8 FUNCTION fun(x)
    IMPLICIT NONE
    REAL*8 x
    fun = 1000.0 * SIN(5.0 * x)
    END

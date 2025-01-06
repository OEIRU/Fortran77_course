PROGRAM LAB4
    REAL A, B, INTEGRAL_TRAPEZE, INTEGRAL_GAUSS
    INTEGER N
    PARAMETER (NMAX = 1000)  ! Максимальное количество узлов сетки
    REAL GRID(NMAX)          ! Одномерный массив для сетки
    EXTERNAL FUN             ! Интегрируемая функция

    ! Ввод данных с клавиатуры
    PRINT *, 'Enter lower limit (A):'
    READ *, A
    PRINT *, 'Enter upper limit (B):'
    READ *, B
    PRINT *, 'Enter number of grid points (N):'
    READ *, N

    ! Проверка на корректность введенных данных
    IF (N .GT. NMAX) THEN
        PRINT *, 'Error: N exceeds maximum allowed value (', NMAX, ')'
        STOP
    END IF
    IF (A .GE. B) THEN
        PRINT *, 'Error: A must be less than B'
        STOP
    END IF

    ! Построение сетки
    CALL BUILD_GRID(A, B, N, GRID)

    ! Вычисление интеграла методом трапеций
    CALL TRAPEZE_METHOD(GRID, N, INTEGRAL_TRAPEZE)
    PRINT *, 'Integral (Trapeze): ', INTEGRAL_TRAPEZE

    ! Вычисление интеграла методом Гаусса-4
    CALL GAUSS4_METHOD(GRID, N, INTEGRAL_GAUSS)
    PRINT *, 'Integral (Gauss-4): ', INTEGRAL_GAUSS

    END

    ! Интегрируемая функция
    REAL FUNCTION FUN(X)
    REAL X
    FUN = X**2 + SIN(X)  ! Пример функции: x^2 + sin(x)
    RETURN
    END

    ! Подпрограмма для построения равномерной сетки
    SUBROUTINE BUILD_GRID(A, B, N, GRID)
    REAL A, B, GRID(N)
    INTEGER N, I
    REAL H
    H = (B - A) / (N - 1)
    DO I = 1, N
        GRID(I) = A + (I - 1) * H
    END DO
    RETURN
    END

    ! Подпрограмма для метода трапеций
    SUBROUTINE TRAPEZE_METHOD(GRID, N, INTEGRAL)
    REAL GRID(N), INTEGRAL
    INTEGER N, I
    REAL H, SUM
    H = GRID(2) - GRID(1)
    SUM = 0.0
    DO I = 2, N - 1
        SUM = SUM + FUN(GRID(I))
    END DO
    INTEGRAL = H * (0.5 * (FUN(GRID(1)) + FUN(GRID(N))) + SUM)
    RETURN
    END

    ! Подпрограмма для метода Гаусса-4
    SUBROUTINE GAUSS4_METHOD(GRID, N, INTEGRAL)
    REAL GRID(N), INTEGRAL
    INTEGER N, I
    REAL H, X1, X2, X3, X4, W1, W2, W3, W4
    PARAMETER (W1 = 0.3478548451, W2 = 0.6521451549,
   &           W3 = 0.6521451549, W4 = 0.3478548451)
    PARAMETER (X1 = -0.8611363116, X2 = -0.3399810436,
   &           X3 = 0.3399810436, X4 = 0.8611363116)
    H = GRID(2) - GRID(1)
    INTEGRAL = 0.0
    DO I = 1, N - 1
        INTEGRAL = INTEGRAL + H / 2.0 * (
   &        W1 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X1 +
   &                 (GRID(I+1) + GRID(I)) / 2.0) +
   &        W2 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X2 +
   &                 (GRID(I+1) + GRID(I)) / 2.0) +
   &        W3 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X3 +
   &                 (GRID(I+1) + GRID(I)) / 2.0) +
   &        W4 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X4 +
   &                 (GRID(I+1) + GRID(I)) / 2.0))
    END DO
    RETURN
    END
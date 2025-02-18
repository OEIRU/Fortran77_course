      PROGRAM main
      REAL a, b
      EXTERNAL fun, fun_wrapper
      a = 0.0
      b = 1.0
      ! Верификация для метода трапеций
      CALL verification_table('Trapezoid', 2, a, b)
      ! Верификация для метода Гаусса-4
      CALL verification_table('Gauss-4', 8, a, b)
      END

      SUBROUTINE verification_table(method_name, k, a, b)
      CHARACTER*(*) method_name
      INTEGER k
      REAL a, b
      REAL integral, integral_2h, analytic, error, error_2h
      REAL ratio, runge_error, richardson, richardson_error
      INTEGER degree, n_current
      INTEGER degrees(15), n_values(15)
      INTEGER i
      EXTERNAL trapezoid_rule, gauss4_rule, fun_wrapper

      ! Массивы для хранения комбинаций degree и N
      DATA degrees /1,2,2,3,3,4,4,7,7,8,8,9,9,10,10/
      DATA n_values /1,1,2,1,2,1,2,1,2,1,2,1,2,1,2/

      PRINT *, 'Method: ', method_name
      PRINT *, 'Order of method: ', k
      PRINT *, '-------------------------------------------'
      DO i = 1, 15
        degree = degrees(i)
        n_current = n_values(i)
        analytic = analytic_integral(a, b, degree)

        IF (method_name .EQ. 'Trapezoid') THEN
          CALL trapezoid_rule(a, b, n_current, integral, degree)
        ELSE IF (method_name .EQ. 'Gauss-4') THEN
          CALL gauss4_rule(a, b, n_current, integral, degree)
        END IF

        error = ABS(analytic - integral)

        IF (method_name .EQ. 'Trapezoid') THEN
          CALL trapezoid_rule(a, b, n_current * 2, integral_2h, degree)
        ELSE IF (method_name .EQ. 'Gauss-4') THEN
          CALL gauss4_rule(a, b, n_current * 2, integral_2h, degree)
        END IF

        error_2h = ABS(analytic - integral_2h)

        IF (error .NE. 0.0) THEN
          ratio = error_2h / error
        ELSE
          ratio = 0.0
        END IF

        runge_error = error_2h / (2**k - 1.0)
        richardson = integral + (integral - integral_2h) / (2**k - 1.0)
        richardson_error = ABS(analytic - richardson)

        PRINT *, 'Degree: ', degree
        PRINT *, 'N: ', n_current
        PRINT *, 'Analytic: ', analytic
        PRINT *, 'Numeric: ', integral
        PRINT *, 'Error: ', error
        PRINT *, 'Ratio: ', ratio
        PRINT *, 'Runge Error: ', runge_error
        PRINT *, 'Richardson: ', richardson
        PRINT *, 'Richardson Error: ', richardson_error
        PRINT *, '-------------------------------------------'
      END DO
      END

      ! Метод трапеций
      SUBROUTINE trapezoid_rule(a, b, n, integral, degree)
      REAL a, b, integral, h, x
      INTEGER i, n
      REAL fun_wrapper
      EXTERNAL fun_wrapper

      integral = 0.0
      h = (b - a) / REAL(n)

      DO i = 0, n-1
        x = a + i * h
        integral = integral + (fun_wrapper(x, degree) 
     & + fun_wrapper(x + h, degree)) * h / 2.0
      END DO
      END

      ! Метод Гаусса-4
      SUBROUTINE gauss4_rule(a, b, n, integral, degree)
      REAL a, b, integral, h, a_i, b_i, mid, t_j
      REAL w(4), xi(4)
      INTEGER i, j, n
      REAL fun_wrapper
      EXTERNAL fun_wrapper

      ! Веса и узлы для метода Гаусса-4
      DATA w /0.3478548451, 0.6521451549, 0.6521451549, 0.3478548451/
      DATA xi /-0.8611363116, -0.3399810436, 0.3399810436, 0.8611363116/

      integral = 0.0
      h = (b - a) / REAL(n)

      DO i = 0, n-1
        a_i = a + i * h
        b_i = a_i + h
        mid = (a_i + b_i) / 2.0
        DO j = 1, 4
          t_j = mid + (h / 2.0) * xi(j)
          integral = integral + w(j) *
     &            fun_wrapper(t_j, degree) * (h / 2.0)
        END DO
      END DO
      END

      ! Функция для вычисления аналитического значения интеграла
      REAL FUNCTION analytic_integral(a, b, degree)
      REAL a, b
      INTEGER degree

      IF (degree .EQ. 0) THEN
        analytic_integral = b - a
      ELSE
        analytic_integral = (b**(degree+1) - 
     &  a**(degree+1)) / REAL(degree + 1)
      END IF
      END

      ! Обертка для функции fun
      REAL FUNCTION fun_wrapper(x, degree)
      REAL x
      INTEGER degree
      REAL fun
      EXTERNAL fun

      fun_wrapper = fun(x, degree)
      END

      ! Интегрируемая функция (полином степени degree)
      REAL FUNCTION fun(x, degree)
      REAL x
      INTEGER degree

      IF (degree .EQ. 1) THEN
        fun = x
      ELSE IF (degree .EQ. 2) THEN
        fun = x**2
      ELSE IF (degree .EQ. 3) THEN
        fun = x**3
      ELSE IF (degree .EQ. 4) THEN
        fun = x**4
      ELSE IF (degree .EQ. 7) THEN
        fun = x**7
      ELSE IF (degree .EQ. 8) THEN
        fun = x**8
      ELSE IF (degree .EQ. 9) THEN
        fun = x**9
      ELSE IF (degree .EQ. 10) THEN
        fun = x**10
      ELSE
        fun = 0.0
      END IF
      END
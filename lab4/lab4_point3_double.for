      PROGRAM MAIN
      IMPLICIT NONE
      REAL*8 A, B, EXACT1, EXACT2
      INTEGER TRAPEZOID, GAUSS4
      PARAMETER (TRAPEZOID=1, GAUSS4=2)
      PARAMETER (A=0.0D0, B=1.0D0)  ! Integration limits

      ! Exact values of integrals
      EXACT1 = 1.0D0 / 11.0D0        ! For x^10
      EXACT2 = (COS(0.0D0) - COS(10.0D0)) / 10.0D0  ! For sin(10x)

      PRINT *, 'Testing x^5:'
      CALL TEST_INTEGRATION(TRAPEZOID, A, B, EXACT1, 1)
      CALL TEST_INTEGRATION(GAUSS4, A, B, EXACT1, 1)

      PRINT *, 'Testing sin(10x):'
      CALL TEST_INTEGRATION(TRAPEZOID, A, B, EXACT2, 2)
      CALL TEST_INTEGRATION(GAUSS4, A, B, EXACT2, 2)

      END PROGRAM MAIN

      REAL*8 FUNCTION FUNC1(X)
      IMPLICIT NONE
      REAL*8 X
      FUNC1 = X**10
      RETURN
      END

      REAL*8 FUNCTION FUNC2(X)
      IMPLICIT NONE
      REAL*8 X
      FUNC2 = SIN(10.0D0 * X)
      RETURN
      END

      SUBROUTINE TEST_INTEGRATION(METHOD, A, B, EXACT, FUNC_SEL)
      IMPLICIT NONE
      INTEGER METHOD, FUNC_SEL
      REAL*8 A, B, EXACT
      INTEGER N, N_STEP, N_MAX, I
      PARAMETER (N_MAX=40)  ! Maximum number of doublings (N = 2^24)
      REAL*8 INTEGRAL, ERROR, PREV_ERROR, RATIO, ORDER

      PRINT *, '-----------------------------------------------'
      IF (METHOD .EQ. 1) THEN
          PRINT *, 'Trapezoid rule (order 2):'
      ELSE IF (METHOD .EQ. 2) THEN
          PRINT *, 'Gauss4 rule (order 8):'
      END IF
      PRINT *, '-----------------------------------------------'
      PRINT *, '        N |     Integral |     Error  | Ratio  | Order'

      PREV_ERROR = 0.0D0
      DO I = 0, N_MAX
          N = 2**I  ! N increases as powers of 2: 1, 2, 4, 8, ...
          IF (METHOD .EQ. 1) THEN
              CALL TRAPEZOID_RULE(A, B, N, INTEGRAL, FUNC_SEL)
          ELSE IF (METHOD .EQ. 2) THEN
              CALL GAUSS4_RULE(A, B, N, INTEGRAL, FUNC_SEL)
          END IF

          ERROR = ABS(INTEGRAL - EXACT)
          IF (I .GT. 0) THEN
              RATIO = PREV_ERROR / ERROR
              ORDER = LOG(RATIO) / LOG(2.0D0)  ! Theoretical order
          ELSE
              RATIO = 0.0D0
              ORDER = 0.0D0
          END IF
          PRINT '(I10, " | ", F12.8, " | ", E10.3, " | ", F5.2, " | "
     &            , F5.2)',
     &          N, INTEGRAL, ERROR, RATIO, ORDER

          ! Stop if error stops decreasing for 3 steps
          IF (I .GT. 3 .AND. ORDER .LT. 0.5) EXIT
          PREV_ERROR = ERROR
      END DO
      PRINT *, '-----------------------------------------------'
      END SUBROUTINE TEST_INTEGRATION

      SUBROUTINE TRAPEZOID_RULE(A, B, N, INTEGRAL, FUNC_SEL)
      IMPLICIT NONE
      INTEGER N, FUNC_SEL
      REAL*8 A, B, INTEGRAL, H, X, SUM
      REAL*8 FUNC1, FUNC2
      INTEGER I

      H = (B - A) / N
      SUM = 0.0D0
      DO I = 1, N-1
          X = A + I * H
          IF (FUNC_SEL .EQ. 1) THEN
              SUM = SUM + FUNC1(X)
          ELSE
              SUM = SUM + FUNC2(X)
          END IF
      END DO
      IF (FUNC_SEL .EQ. 1) THEN
          INTEGRAL = H * (0.5D0 * (FUNC1(A) + FUNC1(B)) + SUM)
      ELSE
          INTEGRAL = H * (0.5D0 * (FUNC2(A) + FUNC2(B)) + SUM)
      END IF
      RETURN
      END

      SUBROUTINE GAUSS4_RULE(A, B, N, INTEGRAL, FUNC_SEL)
      IMPLICIT NONE
      INTEGER N, FUNC_SEL
      REAL*8 A, B, INTEGRAL, H, X, SUM
      REAL*8 WEIGHTS(4), NODES(4)
      REAL*8 FUNC1, FUNC2
      INTEGER I, J

      DATA WEIGHTS /0.347854845137454D0, 0.652145154862546D0,
     &              0.652145154862546D0, 0.347854845137454D0/
      DATA NODES /-0.861136311594053D0, -0.339981043584856D0,
     &             0.339981043584856D0, 0.861136311594053D0/

      H = (B - A) / N
      SUM = 0.0D0
      DO I = 1, N
          DO J = 1, 4
              X = A + (I - 1)*H + 0.5D0 * H * (1.0D0 + NODES(J))
              IF (FUNC_SEL .EQ. 1) THEN
                  SUM = SUM + WEIGHTS(J) * FUNC1(X)
              ELSE
                  SUM = SUM + WEIGHTS(J) * FUNC2(X)
              END IF
          END DO
      END DO
      INTEGRAL = 0.5D0 * H * SUM
      RETURN
      END
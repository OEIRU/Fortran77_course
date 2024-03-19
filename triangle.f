      PROGRAM MAIN
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi

      INTEGER i5
      REAL minAngle, minCos
      pi = 3.14159265
      DO WHILE (i .NE. 5)
      CALL menu()
      CALL input(i)
      SELECT CASE (i)
      CASE (1)
      CALL setData()
      CALL calculateBeta()
      IF (c.GT.0 .AND. alpha.LT.180 .AND. gamma.LT.180 .AND. 
     * alpha+gamma.LT.180) THEN
c      IF (a.LT.b+c .AND. b.LT.a+c .AND. c.LT.a+b) THEN
      CALL calculateS()
      ELSE
      PRINT *, 'Данная фигура не может быть треугольником.'
      END IF
      CASE (2)
      CALL output('S = ', S)
      CASE (3)
      CALL output('Минимальный угол = ', minAngle())
      CASE (4)
      CALL output('Min cos = ', minCos())
      CASE ( : 0, 6 : )
      PRINT *, 'Неверный ввод.'
      END SELECT
      PRINT *, ' '
      END DO
      
      END

      SUBROUTINE menu()
      PRINT *, 'Выберите пункт:'
      PRINT *, '1. Ввод нового треугольника.'
      PRINT *, '2. Вычислить площадь треугольника.'
      PRINT *, '3. Минимальный угол в градусах.'
      PRINT *, '4. Косинус минимального угла.'
      PRINT *, '5. Выход.'
      END

      SUBROUTINE input(item)
      INTEGER item
      PRINT *, ' '
      PRINT *, 'Выберите пункт:'
      READ *, item
      END

      SUBROUTINE output(text, result)
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      CHARACTER *(*) text
      REAL result
c      IF (S .NE. 0) THEN
      PRINT *, text, result
c      ELSE
c      PRINT *, 'Данная фигура не может быть треугольником.'
c      END IF
      END

      SUBROUTINE setData()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      PRINT *, 'Введите данные:'
      PRINT *, 'Сторона: '
      READ *, c
      PRINT *, 'Прилежащий угол: '
      READ *, alpha
      PRINT *, 'Противоположный угол: '
      READ *, gamma 
      END

      SUBROUTINE calculateBeta()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      beta = 180 - alpha - gamma
c      a = c*sin(alpha * pi / 180)/sin(gamma * pi / 180)
c      b = c*sin(beta * pi / 180)/sin(gamma * pi / 180)
      END

      SUBROUTINE calculateS()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
c      S=(c**2)*sin(alpha*pi/180)*sin(beta*pi/180)/(2*sin(gamma*pi/180))
      S=((c**2)/2)*(1-(cos(alpha*pi/180))**2)*(1/(tan(gamma*pi/180))+
     * 1/(tan(alpha*pi/180)))
      END

      REAL FUNCTION minAngle()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      minAngle = min(alpha, beta, gamma)
      END

      REAL FUNCTION minCos()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      minCos = cos(min(alpha, beta, gamma) * pi / 180)
      END


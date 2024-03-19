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
      PRINT *, '������ 䨣�� �� ����� ���� ��㣮�쭨���.'
      END IF
      CASE (2)
      CALL output('S = ', S)
      CASE (3)
      CALL output('��������� 㣮� = ', minAngle())
      CASE (4)
      CALL output('Min cos = ', minCos())
      CASE ( : 0, 6 : )
      PRINT *, '������ ����.'
      END SELECT
      PRINT *, ' '
      END DO
      
      END

      SUBROUTINE menu()
      PRINT *, '�롥�� �㭪�:'
      PRINT *, '1. ���� ������ ��㣮�쭨��.'
      PRINT *, '2. ���᫨�� ���頤� ��㣮�쭨��.'
      PRINT *, '3. ��������� 㣮� � �ࠤ���.'
      PRINT *, '4. ��ᨭ�� �������쭮�� 㣫�.'
      PRINT *, '5. ��室.'
      END

      SUBROUTINE input(item)
      INTEGER item
      PRINT *, ' '
      PRINT *, '�롥�� �㭪�:'
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
c      PRINT *, '������ 䨣�� �� ����� ���� ��㣮�쭨���.'
c      END IF
      END

      SUBROUTINE setData()
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ c
      COMMON /shared/ S, pi
      PRINT *, '������ �����:'
      PRINT *, '��஭�: '
      READ *, c
      PRINT *, '�ਫ���騩 㣮�: '
      READ *, alpha
      PRINT *, '��⨢�������� 㣮�: '
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


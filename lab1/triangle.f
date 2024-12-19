      PROGRAM MAIN
      ! ������� ���� ������ ��� ������ ����묨 ����� ����ணࠬ����.
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi

      REAL minAngle, minCos
      pi = 3.14159265
      DO WHILE (i .NE. 5)
      CALL menu()      ! �맮� ����ணࠬ�� ��� �뢮�� ����
      CALL input(i)    ! �맮� ����ணࠬ�� ��� �뢮�� �롮� ���짮��⥫�
      SELECT CASE (i)  ! ��।������ ����権 switch
      
      CASE (1)
      CALL setData()
      CALL calculateGamma()
      IF (a.GT.0 .AND. b.GT.0 .AND. alpha.LT.180) THEN
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
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi
      CHARACTER *(*) text
      REAL result
c      IF (S .NE. 0) THEN
      PRINT *, text, result
c      ELSE
c      PRINT *, '������ 䨣�� �� ����� ���� ��㣮�쭨���.'
c      END IF
      END

      SUBROUTINE setData() ! ���� ������ 
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      PRINT *, '������ �����:'
      PRINT *, '��஭� a: '
      READ *, a
      PRINT *, '��஭� b: '
      READ *, b
      PRINT *, '���� ����� ����: '
      READ *, alpha
      END

      SUBROUTINE calculateGamma() ! ���᫥��� 㣫��
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      c = sqrt(a**2+b**2-2*a*b*cos(alpha * pi / 180))
      beta = acos((b**2 + c**2 - a**2)/(2*b*c)) * 180 / pi  
      gamma = 180 - alpha - beta 
      PRINT *, '���祭�� ��६����� a:', a 
      PRINT *, '���祭�� ��६����� b:', b 
      PRINT *, '���祭�� ��६����� �:', c 
      PRINT *, '���祭�� ��६����� alpha:', alpha
      PRINT *, '���祭�� ��६����� beta:', beta
      PRINT *, '���祭�� ��६����� gamma:', gamma
      END
 
      SUBROUTINE calculateS() ! ���᫥��� ���頤� 
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      S = 0.5 * a * b * sin(alpha * pi / 180)
      END

      REAL FUNCTION minAngle() ! ���� �������쭮�� 㣫�
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      minAngle = min(alpha, beta, gamma) 
      END

      REAL FUNCTION minCos() ! ���� �������쭮�� ��ᨭ��
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi
      minCos = cos(min(alpha, beta, gamma) * pi / 180)
      END


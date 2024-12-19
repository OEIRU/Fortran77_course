      PROGRAM main
      dimension AMem(200000000)
      
      print *, 'Enter a, b, N and number of iterations'
      read *, a, b, N, k
      
      n = N+1
      n_start = n
      
      ! Метод трапеций
      n = n_start
      print *, 'Trapezoid:'
      DO i=1,k
         call generate_grid(a, b, n, AMem)
         result = Trapezoid(AMem, n)
         print '(F20.15)', result
         n = (n-1) * 2 + 1
      ENDDO
      
      ! Метод Гаусса (n = 2)
      n = n_start
      print *, 'Gauss (n = 2):'
      DO i=1,k
         call generate_grid(a, b, n, AMem)
         result = Gauss2(AMem, n)
         print '(F20.15)', result
         n = (n-1) * 2 + 1
      ENDDO
      
      pause
      END
      
      
      SUBROUTINE generate_grid(a,b,n,grid)
      dimension grid(n)
      
      DO i=1,n
         grid(i) = a + (i-1)*(b-a)/(n-1)
      ENDDO
      
      END
      
      
      FUNCTION fun(x)
      !fun = 1.23*x**3 + 4.56*x**2 + 7.89*x + 1.23
      fun = 4 * x**3
      return
      END
      
      
      FUNCTION Trapezoid(grid, n)
      dimension grid(n)
      
      result = 0
      DO i=1,n-1
         result = result + (fun(grid(i)) + fun(grid(i+1)))
      ENDDO
      
      Trapezoid = 0.5 * (grid(n)-grid(1))/(n-1) * result
      return
      END
      
      
      FUNCTION Gauss2(grid, n)
      dimension grid(n)
      
      x1 = -1.0 / 3**0.5
      x2 =  1.0 / 3**0.5
      w1 = 1.0
      w2 = 1.0
      
      h = (grid(n)-grid(1))/(n-1)
      
      result = 0
      DO i=1,n-1
         mid = 0.5 * (grid(i) + grid(i+1))
         half_h = 0.5 * h
         result = result + w1 * fun(mid + half_h * x1) + 
     &            w2 * fun(mid + half_h * x2)
      ENDDO
      
      Gauss2 = 0.5 * h * result
      return
      END


      
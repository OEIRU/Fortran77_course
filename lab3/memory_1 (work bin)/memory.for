**Главная программа**
	program memory
	parameter(n=1000000)
	dimension A(n)
	common/razm/i,i1,i2
	call vvod(A(1),n)
	call multiplying(A(1),A(1+i),A(2*i),A(3*i-1),A(4*i-i1-1),
     *A(5*i-2*i1-1),A(6*i-2*i1-i2-1),A(7*i-2*i1-2*i2-1),
     *A(8*i-2*i1-2*i2-1),i,i1,i2)
	call output(A(8*i-2*i1-2*i2-1),i)
	stop
	end

**Ввод данных**
	subroutine vvod(A,n)
	common/razm/i,i1,i2
	dimension A(n)
	open(1,FILE='matrix.bin',ACCESS='direct',RECL=12)	
*Ввод размерности матрицы и смещений диагоналей
	read(1,rec=1)i,i1,i2
*Проверка матрицы на корректность
	if(i1.le.1.or.i2.le.i1.or.i.le.i1)then
		print*,'The matrix is given not correctly!'
		stop
	endif
	open(2,FILE='vector.bin',ACCESS='direct',RECL=4)	
*Ввод размерности вектора
	read(2,rec=1)m
*Проверка вектора на коррекность
	if(i.ne.m)then
		print*,'The vector is given not correctly'
		stop
	endif
*Проверка на нехватку памяти 
	if(9*i-2*i1-2*i2-2>n)then 
		print*,'Shortage of memory'
		stop
	endif
	call readdata(A(1),A(1+i),A(2*i),A(3*i-1),A(4*i-i1-1),
     *A(5*i-2*i1-1),A(6*i-2*i1-i2-1),A(7*i-2*i1-2*i2-1),i,i1,i2)
	return
	end

**Ввод данных**
	subroutine readdata(A,B1,B2,C1,C2,D1,D2,E,k,l,m)	
	dimension A(k),B1(k-1),B2(k-1),C1(k-l),C2(k-l),D1(k-m),D2(k-m),E(k)	
	open(1,FILE='matrix.bin',ACCESS='direct',RECL=4)	
*Ввод матрицы
	do i=1,k
		read(1,rec=3+i)A(i)
	end do
	do i=1,k-1
		read(1,rec=3+k+i)B1(i)
	end do
	do i=1,k-1
		read(1,rec=3+2*k-1+i)B2(i)
	end do
	do i=1,k-l
		read(1,rec=3+3*k-l-1+i)C1(i)
	end do
	do i=1,k-l
		read(1,rec=3+4*k-2*l-1+i)C2(i)
	end do
	do i=1,k-m
		read(1,rec=3+4*k-2*l-m-1+i)D1(i)
	end do
	do i=1,k-m
		read(1,rec=3+4*k-2*l-2*m-1+i)D2(i)
	end do
*Ввод вектора
	do i=1,k
		read(2,rec=1+i)E(i)
	end do	
	return
	end
	
**Умножение матрицы на вектор**
	subroutine multiplying(A,B1,B2,C1,C2,D1,D2,E,F,k,l,m)
	dimension A(k),B1(k-1),B2(k-1),C1(k-l),C2(k-l)
	dimension D1(k-m),D2(k-m),E(k),F(k)
	F(1)=(A(1)+B1(1))*E(1)
	F(k)=(B2(k-1)+A(k))*E(k)	
	do i=2,k-1
		F(i)=(B2(i-1)+A(i)+B1(i))*E(i)
	end do
	do i=1,k-m
		F(i)=F(i)+(C1(i)+D1(i))*E(i)
	end do
	do i=i,k-l
		F(i)=F(i)+C1(i)*E(i)
	end do
	do i=l+1,m
		F(i)=F(i)+C2(i-l)*E(i)
	end do
	do i=i,k
		F(i)=F(i)+(D2(i-m)+C2(i-l))*E(i)
	end do
	return
	end

**Вывод результирующего вектора**
	subroutine output(A,k)
	dimension A(k)
	open(1,file='result.bin',ACCESS='direct',RECL=4)
	do i=1,k
		write(1,rec=i)A(i)
	end do
	return
	end
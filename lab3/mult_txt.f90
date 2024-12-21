module matrix_multiply
    implicit none
    integer, parameter :: max_size = 1000000
    integer :: n, i1, i2
    real :: A(5, max_size)  ! Хранение диагоналей в двумерном массиве
    real :: F(max_size), result(max_size)
    common /razm/ n, i1, i2

contains

subroutine read_matrix(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(10, file=filename, status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(10, *, iostat=io) n, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read dimensions'
        stop
    end if
    
    ! Чтение всех диагоналей
    read(10, *, iostat=io) (A(1, i), i=1, n)    ! Главная диагональ
    read(10, *, iostat=io) (A(2, i), i=1, n-1)  ! Первая верхняя
    read(10, *, iostat=io) (A(3, i), i=1, n-i2) ! Вторая верхняя
    read(10, *, iostat=io) (A(4, i), i=1, n-1)  ! Первая нижняя
    read(10, *, iostat=io) (A(5, i), i=1, n-i1) ! Вторая нижняя

    if (io /= 0) then
        print *, 'Error: Failed to read matrix data'
        stop
    end if
    close(10)

end subroutine read_matrix

subroutine read_vector(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(11, file=filename, status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(11, *, iostat=io) n
    if (io /= 0) then
        print *, 'Error: Failed to read vector size'
        stop
    end if

    read(11, *, iostat=io) (F(i), i=1, n)
    if (io /= 0) then
        print *, 'Error: Failed to read vector data'
        stop
    end if
    close(11)
end subroutine read_vector

subroutine multiply_matrix_vector()
    integer :: i
    
    do i = 1, n
        result(i) = A(1, i) * F(i)
        
        if (i > 1) result(i) = result(i) + A(4, i-1) * F(i-1)  ! Первая нижняя диагональ
        if (i > 2) result(i) = result(i) + A(5, i-2) * F(i-2)  ! Вторая нижняя диагональ
        
        if (i < n) result(i) = result(i) + A(2, i) * F(i+1)  ! Первая верхняя диагональ
        if (i <= n-i2) result(i) = result(i) + A(3, i) * F(i+i2)  ! Вторая верхняя диагональ
    end do

    F(1:n) = result(1:n)
end subroutine multiply_matrix_vector

subroutine write_vector(filename)
    character(len=*), intent(in) :: filename
    integer :: io
    
    open(12, file=filename, status='replace', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot write to file ', filename
        stop
    end if
    
    write(12, *) F(1:n)
    close(12)
end subroutine write_vector

end module matrix_multiply

program main
    use matrix_multiply
    
    call read_matrix('matrix.txt')
    call read_vector('vector.txt')
    call multiply_matrix_vector()
    call write_vector('result_1.txt')
    
    print *, 'Multiplication completed successfully.'
end program main

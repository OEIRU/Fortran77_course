module memory_module
    implicit none
    integer, parameter :: n = 1000000
    integer :: i, i1, i2
    real :: A(n), F(n)
    common /razm/ i, i1, i2

contains

subroutine vvod(A, n)
    implicit none
    real, dimension(:), intent(out) :: A
    integer, intent(in) :: n
    integer :: m, io
    integer :: k
    character(len=256) :: line

    open(10, FILE='matrix.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open matrix.txt'
        stop
    endif

    read(10, '(A)', IOSTAT=io) line
    read(line, *) i, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read matrix dimensions'
        stop
    endif

    if (i1 <= 1 .or. i2 <= i1 .or. i <= i2) then
        print *, 'Error: Invalid matrix dimensions: i =', i, ', i1 =', i1, ', i2 =', i2
        stop
    endif

    if (5 * i - 2 * i1 - 2 * i2 - 2 > n) then
        print *, 'Error: Insufficient memory'
        stop
    endif

    ! Reading main diagonal
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(k), k = 1, i)
    print *, 'Main diagonal:', (A(k), k = 1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read main diagonal'
        stop
    endif

    ! Reading first upper diagonal
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(i + k), k = 1, i - 1)
    print *, 'First upper diagonal:', (A(i + k), k = 1, i - 1)
    if (io /= 0) then
        print *, 'Error: Failed to read first upper diagonal'
        stop
    endif

    ! Reading second upper diagonal
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(2 * i - 1 + k), k = 1, i - i2)
    print *, 'Second upper diagonal:', (A(2 * i - 1 + k), k = 1, i - i2)
    if (io /= 0) then
        print *, 'Error: Failed to read second upper diagonal'
        stop
    endif

    ! Reading first lower diagonal
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(3 * i - 2 + (i - i2) + k), k = 1, i - 1)
    print *, 'First lower diagonal:', (A(3 * i - 2 + (i - i2) + k), k = 1, i - 1)
    if (io /= 0) then
        print *, 'Error: Failed to read first lower diagonal'
        stop
    endif

    ! Reading second lower diagonal
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(4 * i - 3 + (i - i2) + k), k = 1, i - i1)
    print *, 'Second lower diagonal:', (A(4 * i - 3 + (i - i2) + k), k = 1, i - i1)
    if (io /= 0) then
        print *, 'Error: Failed to read second lower diagonal'
        stop
    endif

    close(10)

    ! Reading vector F
    open(11, FILE='vector.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open vector.txt'
        stop
    endif

    read(11, *, IOSTAT=io) m
    if (io /= 0) then
        print *, 'Error: Failed to read vector size'
        stop
    endif

    if (i /= m) then
        print *, 'Error: Vector size does not match matrix: i =', i, ', m =', m
        stop
    endif

    read(11, *, IOSTAT=io) (F(k), k = 1, i)
    print *, 'Vector F:', (F(k), k = 1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read vector elements'
        stop
    endif

    close(11)
end subroutine vvod

subroutine multiplying_classical(A, F, i, i1, i2)
    implicit none
    real, dimension(:), intent(in) :: A
    real, dimension(:), intent(inout) :: F
    integer, intent(in) :: i, i1, i2
    integer :: k
    real :: temp(n)  ! Temporary vector for storing result

    temp = 0.0  ! Initialize temp vector with zeros

    print *, "Debug: Starting matrix-vector multiplication"

    ! Multiply main diagonal
    do k = 1, i
        temp(k) = temp(k) + A(k) * F(k)
        print *, "Main diag: temp(", k, ") += A(", k, ") * F(", k, ") = ", &
                 A(k), "*", F(k)
    end do

    ! Multiply first upper diagonal
    do k = 1, i - 1
        temp(k + 1) = temp(k + 1) + A(i + k) * F(k)
        print *, "Upper1 diag: temp(", k + 1, ") += A(", i + k, ") * F(", k, ") = ", &
                 A(i + k), "*", F(k)
    end do

    ! Multiply second upper diagonal
    do k = 1, i - i2
        temp(k + i2) = temp(k + i2) + A(2 * i - 1 + k) * F(k)
        print *, "Upper2 diag: temp(", k + i2, ") += A(", 2 * i - 1 + k, ") * F(", k, ") = ", &
                 A(2 * i - 1 + k), "*", F(k)
    end do

    ! Multiply first lower diagonal
    do k = 2, i
        temp(k) = temp(k) + A(3 * i - 2 + k - 1) * F(k - 1)
        print *, "Lower1 diag: temp(", k, ") += A(", 3 * i - 2 + k - 1, ") * F(", k - 1, ") = ", &
                 A(3 * i - 2 + k - 1), "*", F(k - 1)
    end do

    ! Multiply second lower diagonal
    do k = i1 + 1, i
        temp(k) = temp(k) + A(4 * i - 3 + k - i1 - 1) * F(k - i1)
        print *, "Lower2 diag: temp(", k, ") += A(", 4 * i - 3 + k - i1 - 1, ") * F(", k - i1, ") = ", &
                 A(4 * i - 3 + k - i1 - 1), "*", F(k - i1)
    end do

    print *, "Debug: Finished matrix-vector multiplication"

    F(1:i) = temp(1:i)  ! Save result to F vector
end subroutine multiplying_classical

subroutine output(F, i)
    implicit none
    real, dimension(:), intent(in) :: F
    integer, intent(in) :: i
    integer :: io

    open(12, FILE='result.txt', STATUS='unknown', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open result.txt for writing'
        stop
    endif

    write(12, *) F(1:i)

    close(12)
end subroutine output

end module memory_module

program matrix_vector_debug
    use memory_module
    implicit none

    call vvod(A, n)
    call multiplying_classical(A, F, i, i1, i2)
    call output(F, i)

end program matrix_vector_debug

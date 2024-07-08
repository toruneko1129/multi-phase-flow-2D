program main
implicit none
include 'mpif.h'


!ierr : error status
!nproc: num of process
!ID   : process id
integer :: ierr, nproc, ID


!>mpi init=====================================================================

call mpi_init(ierr)
call mpi_comm_size(mpi_comm_world, nproc, ierr)
call mpi_comm_rank(mpi_comm_world, ID, ierr)

!>mpi finished=================================================================

call mpi_finalize(ierr)


stop
end program main
! F90ETMETADATA

character*71 headline

write(*,*),'reading progress_.json ...'
open(101,file='C:\FILES\EASYTEN\meta_data_.json')
read(101,*), headline
write(*,*), headline
close(101);



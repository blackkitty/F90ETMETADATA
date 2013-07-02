! F90ETMETADATA

character*20   headline(1:7)
integer*4      metadata(1:21864,1:7), regdate(1:21864, 1:2), paydate(1:21864, 1:2) !paydate(:, day:month)
integer*2      regday (1:21864), payday (1:21864), paydaySinceReg (1:21864)
integer*4      distribution(-1:100), daySumDistribution(-1:100)
integer*2      paytype(1:21864), dist33(-1:100), dist299(-1:100), dist699(-1:100), dist1599(-1:100)
integer*4      listRevenue(0:11), listN(0:11)
real           tmp

write(*,*),'reading meta_data_.txt ...'
open(101,file='C:\FILES\EASYTEN\meta_data_.txt')
read(101,*), headline(:)
!write(*,*), headline(:)
do i = 1, 21864
!write(*,*), i
read(101,*), metadata(i,:)
enddo
close(101);

!write(*,*), metadata(21864,:)
regdate(:,1) = metadata(:,4)/1000000
regdate(:,2) = mod(metadata(:,4)/10000, 100)
paydate(:,1) = metadata(:,7)/1000000
paydate(:,2) = mod(metadata(:,7)/10000, 100)
paytype(:)   = metadata(:,6)
do i = 1, 21864
    call time2013(regdate(i,1), regdate(i,2), regday(i))
    if (metadata(i,7) > 0) then
    call time2013(paydate(i,1), paydate(i,2), payday(i))
    paydaySinceReg(i) = payday(i) - regday(i)
    endif
    if (metadata(i,7) == 0) paydaySinceReg(i) = -1 
distribution(paydaySinceReg(i)) = distribution(paydaySinceReg(i)) +1 ! считаем сколько каких дней
daySumDistribution(paydaySinceReg(i)) = daySumDistribution(paydaySinceReg(i)) + paytype(i)
if (paytype(i) == 33) dist33(paydaySinceReg(i)) = dist33(paydaySinceReg(i)) + 1
if (paytype(i) == 299) dist299(paydaySinceReg(i)) = dist299(paydaySinceReg(i)) + 1
if (paytype(i) == 699) dist699(paydaySinceReg(i)) = dist699(paydaySinceReg(i)) + 1
if (paytype(i) == 1599) dist1599(paydaySinceReg(i)) = dist1599(paydaySinceReg(i)) + 1

write(*,*), paydaySinceReg(i)
enddo

! пишем в файл дни от регистрации
write(*,*),'writing statistics.txt ...'
!open(101,file='C:\FILES\EASYTEN\paydaySinceRegDist.txt', status = 'replace')
!open(101,file='C:\FILES\EASYTEN\paidUsersDist.txt', status = 'replace')
!do i = -1, 100
!tmp = 1.0*daySumDistribution(i)/distribution(i)
!if (distribution(i) == 0) tmp = 0
!write(101,'(5i5)'), i, dist33(i), dist299(i), dist699(i), dist1599(i)
!enddo
!close(101);

open(101,file='C:\FILES\EASYTEN\revenueByLists.txt', status = 'replace')
do i = 1, 21864
write(*,*), i
if (metadata(i,2) > 10) then
listN(11) = listN(11) + 1
listRevenue(11) = listRevenue(11) + paytype(i)
endif
if (metadata(i,2) < 11) then
listN(metadata(i,2)) = listN(metadata(i,2)) + 1
listRevenue(metadata(i,2)) = listRevenue(metadata(i,2)) + paytype(i)
endif
enddo

do i = 0, 11
tmp = 1.0*listRevenue(i)/listN(i)
write(101,*), i, listN(i), tmp, listRevenue(i)
enddo
close(101)

pause
END

subroutine time2013(day, month, time) ! определяется порядковый номер дня в году 
byte day, month
integer*2 time
select case (month)
case (3)
time = 31 + 28 + day
case (4)
time = 31 + 28 + 31 + day
case (5)
time = 31 + 28 + 31 + 30 + day
case (6)
time = 31 + 28 + 31 + 30 + 31 + day 
!case (7)
!time = 31 + 28 + 31 + 30 + 31 + 30 + day
!case (8)
!time = 31 + 28 + 31 + 30 + 31 + 30 + 31 + day
!case (9)
!time = 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + day
!case (10)
!time = 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + day
!case (11)
!time = 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + day
endselect

end

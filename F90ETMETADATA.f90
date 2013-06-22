! F90ETMETADATA
parameter progress_len = 9697719
character*progress_len progress
parameter userInfo_len = 15000
character*userInfo_len userInfo
character*10           buf  
integer*4              a, b, users  
integer                wordID                   ! for subroutine parseWordStatus
byte                   wordStatus, colon, comma ! for subroutine parseWordStatus
integer                userID  
byte                   lastDay                  ! for subroutine parseUser
integer*4              userMarkers(1:25000,1:2) ! 1 - begin, 2 - end
                       userMarkers(1,1) = 1
                       ! userMarkers - это массив, содержащий начальные и конечные позиции записей по юзерам в файле  
                                  
                     
a = 0; b = 0; users = 0;

1001 FORMAT(A9697719) ! progress.json
write(*,*),'reading progress_.json ...'
open(101,file='C:\FILES\EASYTEN\progress_.json')
read(101,1001), progress
close(101);

do i = 1, progress_len ! ищем маркеры инфы по юзерам
if (progress(i:i) == '{') a = a + 1
if (progress(i:i) == '}') b = b + 1
if ((progress(i:i) == '}') .and. (a > 0) .and. (a .eq. b)) then
!write(*,*), i;
users = users + 1
userMarkers(users,2)   = i     ! промаркировали конец записи по юзеру
userMarkers(users+1,1) = i + 2 ! начало инфы по сл юзеру - через зап€тую
endif
!write(*,*), i, a, b 
enddo
write(*,*), 'progress.json', a, b, users
a = 0; b = 0; i = 0;

CALL parseWordStatus('"1023":10,"', wordID, wordStatus)
write(*,*),'parseWordStatus:', wordID, wordStatus

open(102,file='C:\FILES\EASYTEN\temp.txt')
do i = 1, users
write(102,*), userMarkers(i,2) - userMarkers(i,1)
enddo
close(102)
!************************************************
! ѕарсим инфо юзера
!************************************************
write(*,*),'parsing users info...'
userInfo = ''
open(102,file='C:\FILES\EASYTEN\usersActivity.txt',status = 'replace')
do i = 1, users
userInfo = progress(userMarkers(i,1):userMarkers(i,2))
CALL parseUser(userInfo, userID, lastDay)
!write(102,*), i, userID, lastDay

enddo

pause
END

SUBROUTINE parseWordStatus(input, wordID, wordStatus) ! »звлекает из кусочков типа '"2623":31,"' отдельно wordID = 2623 и wordStatus = 31  
character*10 input
character*5  buf5
character*2  buf2
byte         delimiter, wordStatus
integer      wordID
do i = 1, 10
if (input(i:i) == ":") colon = i
if (input(i:i) == ",") comma = i
enddo
write(buf5,'(A)'), input(2:colon-2);        read(buf5,'(i5)'), wordID
write(buf2,'(A)'), input(colon+1:comma-1);  read(buf2,'(i2)'), wordStatus
END

SUBROUTINE parseUser(input, userID, lastDay)
integer*4              dayMarkers(1:25000,1:2) ! 1 - begin, 2 - end
                       ! dayMarkers - это массив, содержащий начальные и конечные позиции записей по дн€м в юзеринфо
integer         userID
character*15000 input
character*5     buf5
byte            userIDquotes, lastDay
do i = 10, 2, -1
if (input(i:i) == '"') userIDquotes = i
enddo
dayMarkers(1,1) = userIDquotes + 3
lastDay = 1
! »щем позиции номеров дней - начальные маркеры
do i = dayMarkers(1,1)+10, 15000-1
if ((input(i:i) == "}") .and. (input(i+1:i+1) == ',')) then
lastDay =lastDay + 1
dayMarkers(lastDay, 1) = i+2
endif
enddo
! »щем позиции номеров дней - конечные маркеры
do d = 1, lastDay
    do i = dayMarkers(d, 1), dayMarkers(d, 1) + 10
    if ((input(i:i) == '"') .and. (input(i+2:i+2) == '{')) then
    dayMarkers(d, 2) = i
    endif
    enddo
enddo

write(buf5,'(A)'), input(2:userIDquotes-1); read(buf5,'(i5)'), userID
!write(*,*), 'parseUser: userID ', userID
!do i = 1, lastDay
!write(*,*), dayMarkers(i, 1:2), 'active day ',i, input(dayMarkers(i, 1):dayMarkers(i, 2))
!enddo
!write(*,*), 'lastDay = ', lastDay

END
      program spectra
                 real::a(10,10)=0.0d0,sum=0.0d0,d=0.0d0,e=0.0d0,cdsum=0.0d0
                 real::b=0.00032262d0,c=13.062974d0,p=0.0d0,Ri=0.0d0
                 open(1,file='data.dat')
                 open(2,file='spectrum-tm-abs.dat')
                 do i=1,10
                 read(1,*)(a(i,j),j=1,3)
                 enddo
                 do k=300,1200
                 do l=1,10
                 d=1.0d0/k
                 e=1.0d0/a(l,1)  
                 p=((d-e)/b)
                 Ri=a(l,3)
                 sum=sum+c*(a(l,2)/b)*exp(-p**2)
                 cdsum=cdsum+1.37*(e*Ri/sqrt(b))*exp(-p**2)
                enddo
                write(2,*)k,sum,abs(cdsum)
                sum=0.0d0
                cdsum=0.0d0
                enddo
               end program spectra


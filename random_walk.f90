program caminata
implicit none
real*8::dist,sumi
real*8::dist2,a1,b1,distancia
integer::i,viajes,sig,a2,vago(3),k,b2,vaga(3),sig2,pasos,amor,x1,y1,z1,x2,y2,z2

open(unit=10,file='Caminante1.dat',status='replace')   !Elegante y misterioso
open(unit=20,file='Caminante2.dat',status='replace')   !Alegre y aventurera
open(unit=30,file='DistanciasVSEncuentros.dat',status='replace')
dist=0
dist2=0

do i=1,100 !¨Cu ntas veces realizaremos esto cambiando las distancias?

!POSICIONES INICIALES, UN POCO ALEATORIAS, cambiar esto si el usuario
!quiere establecer las posiciones.
amor=0
x1=NINT(10.0*ran())-NINT(10.0*ran())
y1=NINT(10.0*ran())-NINT(10.0*ran())
z1=NINT(10.0*ran())-NINT(10.0*ran())
x2=NINT(10.0*ran())-NINT(10.0*ran())
y2=NINT(10.0*ran())-NINT(10.0*ran())
z2=NINT(10.0*ran())-NINT(10.0*ran())
distancia=((x1-x2)**2.0+(y1-y2)**2.0+(z1-z2)**2.0)**(0.5)

do viajes=1,100  !El limite del ciclo define cuantas veces llevar n a
                 !cabo la caminata simult nea
vago(1)=x1
vago(2)=y1
vago(3)=z1
vaga(1)=x2
vaga(2)=y2
vaga(3)=z2

do pasos=1,100   !¨Cu ntos pasos ser n libres de dar hasta encontrarse?
a1=ran() !Variable para elegir la direcci¢n del desplazamiento del primer caminante
a2=nINT(ran()) !Para elegir el signo del desplazamiento del primer caminante
!write(*,*)a1,a2
  if (a2==0) then   !Condicional para elegir el signo
  sig=1
  else
  sig=-1
  end if
  
if (a1.LT.1.0/3.0) then   !Condicional para elegir la direcci¢n
vago(1)=vago(1)+sig*1
end if
if (a1.LT.2.0/3.0.and.a1.GT.1.0/3.0) then
vago(2)=vago(2)+sig*1
end if
if (a1.GT.2.0/3.0) then
vago(3)=vago(3)+sig*1
end if

b1=ran()        !Variable para elegir la direcci¢n del desplazamiento del segundo caminante
b2=nINT(ran())   !Para elegir el signo del desplazamiento del segundo caminante

  if (b2==0) then     !Para el signo
  sig2=1
  else
  sig2=-1
  end if
  
if (b1.LT.1.0/3.0) then   !Para la direcci¢n
vaga(1)=vaga(1)+sig2*1
end if
if (b1.LT.2.0/3.0.and.b1.GT.1.0/3.0) then
vaga(2)=vaga(2)+sig2*1
end if
if (b1.GT.2.0/3.0) then
vaga(3)=vaga(3)+sig2*1
end if

!En cada paso se verifica si las posiciones de ambos son iguales, de ser
!as¡, salimos del ciclo repentinamente.
if (vago(1)==vaga(1).and.vago(2)==vaga(2).and.vago(3)==vaga(3)) then
amor=amor+1
write(*,*)'Se encuentran en el viaje',viajes,'al paso',pasos
go to 15
end if

!end do
end do
!write(20,*)vaga(1),vaga(2),vaga(3)
!write(10,*)vago(1),vago(2),vago(3)
15 continue
!dist=(vago(1)**(2.0)+vago(2)**(2.0))**0.5
!dist2=dist2+dist**2.0
!write(10,*)vago(1),vago(2)
!write(10,*)log10(real(j)),log10(sqrt(dist2/10000))
end do
write(30,*)distancia,amor
end do

pause

end program

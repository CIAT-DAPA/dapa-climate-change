%Mesh 1 (complicated way)
clear all;clc;close all
temp=[10,20,30,40]';
rain=[300,800,1500,2500]';

x=[15:1:35]; y=[min(rain):10:2000];
[X,Y] = meshgrid(x,y);
Fk=linspace(20,30,11);
Fk1=linspace(800,1500,11); 
l=1;
for i=1:length(Fk);
    for j=1:length(Fk1);
        point(l,:)=[Fk(i),Fk1(j)];
        l=l+1;
    end
end
RRsg=[point];
[np nl]=size(X);
[np1 nl1]=size(Y);
XX=zeros(np,nl);
YY=zeros(np1,nl1);
[nf nc]=find(X);
RRf=[nf nc];
sig=5;
sig1=0.5;
psig=40;
psig1=20;
st=1;
for i=1:length(RRsg);
        [nfs nfk]=find(X==RRsg(i,1));
        [nfs1 nfk1]=find(Y==RRsg(i,2));
        nf_l=nfs(1);
        nf_k=nfk(1);
        nf_l1=nfs1(1);
        nf_k1=nfk1(1);
        Ks=[nf_l1(1),nf_k(1)];
%         Ks=[nf_k1(1),nf_l(1)];
        for ii=1:length(nf);
                YY(nf(ii),nc(ii))=exp(-((((Ks(1,1)-RRf(ii,1))).^2)/(4*(sig^2)) + (((Ks(1,2)-RRf(ii,2))).^2)/(4*(sig1^2))));
%                 XX(nf(ii),nc(ii))=exp(-((((Ks(1,1)-RRf(ii,1))).^2)/(4*(psig^2)) + (((Ks(1,2)-RRf(ii,2))).^2)/(4*(psig1^2))));
        end;
        Z(:,:,st)=YY;
        Z1=sum(Z,3);
        st=st+1;
    clear ic nfs nfk nfs1 nfk1 YY
end;
Z3=(Z1./max(max((Z1)))).*100;

figure, surf(X,Y,Z3)
colorbar
xlabel('Temperature (ºC)'); ylabel('Precipitation (mm)');
zlabel('Suitability (%)');
axis tight

%% 2nd (simpler) way to do it but needs the image processing toolbox
clear all;

%Input parameters
temp=[10,20,30,40]';
rain=[300,800,1500,2500]';

%X and Y vectors initialization
x=linspace(15,35,200); y=linspace(min(rain),2000,200);
[X,Y] = meshgrid(x,y);

%Do matrices and so on...
I=zeros(200,200);
I(41:160,41:160)=ones(120,120); %Basic binary masl
I = uint8(I.*255); 
H = fspecial('gaussian',50,50);%Filter a bit for nice visualization
J = imfilter(I,H,'replicate'); 
figure, surf(X,Y,100*(double(J)./255),'facecolor','interp','FaceLighting','phong')
colorbar
xlabel('Temperature (ºC)'); ylabel('Precipitation (mm)');
zlabel('Suitability (%)');
axis tight

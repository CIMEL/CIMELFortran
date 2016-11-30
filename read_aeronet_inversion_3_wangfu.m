%读取AERONET反演产品
%%
clear;
stns_fn='hangzhou';
stns_id='808';
YearInCount=2013;
%fout=['h:\CARSNET_INVERSION\CIMEL_NETWORK\' stns_fn '\dubovik\'];
fout=['h:\CIMEL_NETWORK\' stns_fn '\dubovik\'];
if ~exist(fout,'dir')
    mkdir(fout);
end
fpath=['H:\AERONET_INVERSION\output\' stns_fn '\'];
%fpath=['f:\CARSNET_INVERSION\CIMEL_NETWORK\' stns_fn '\'];
strmm=['01';'02';'03';'04';'05';'06';'07';'08';'09';'10';'11';'12'];
strdd=['01';'02';'03';'04';'05';'06';'07';'08';'09';'10';'11';'12';'13';'14';'15';'16';'17';'18';'19';'20';'21';'22';'23';'24';'25';'26';'27';'28';'29';'30';'31'];
fidw=fopen([fout 'Dubovik_stats_' stns_fn '_' stns_id '_20130612.dat'],'w');
fprintf(fidw,'%s',['year,mm,dd,hh,mm,ss,'...
                   'aod440,aod675,aod870,aod1020,'...
                   'extt440,extt670,extt870,extt1020,'...
                   'extf440,extf670,extf870,extf1020,'...
                   'extc440,extc670,extc870,extc1020,'...
                   'ssat440,ssat670,ssat870,ssat1020,'...
                   'ssaf440,ssaf670,ssaf870,ssaf1020,'...
                   'ssac440,ssac670,ssac870,ssac1020,'...%
                   'aaod440,aaod670,aaod870,aaod1020,'...
                   'real440,real670,real870,real1020,'...
                   'imag440,imag670,imag870,imag1020,'...
                   '0.050,0.066,0.086,0.113,0.148,0.194,0.255,'...
                   '0.335,0.439,0.576,0.756,0.992,1.302,1.708,'...
                   '2.241,2.940,3.857,5.051,6.641,8.713,11.43,15.00,'...
                   'refft,refff,reffc,'...
                   'volt,volf,volc,'...
                   'rmeat,rmeaf,rmeac,'...
                   'rstdt,rstdf,rstdc,'...
                   'flxdn1,flxdn2,flxdn3,flxdn4,'...
                   'flxup1,flxup2,flxup3,flxup4,'...
                   'albedo440,albedo670,albedo870,albedo1020,'...
                   'sphere,sunerr,skyerr']);
fprintf(fidw,'\n');
                   
stats_inversion=[];

% m=find(stns_fn=='-');
% if isempty(m)
%     fname=dir([fpath '*' stns_fn '*.dat']);
% else
%     fname=dir([fpath '*' stns_fn(1:m(1)-1) '*.dat']);
% end

fname=dir([fpath '*' stns_fn '*.dat']);

for id=1:length(fname);
% for id=1406:1406;
%========================================================
%initialize variable
    daily_inversion=[];
    ssa=zeros(4,3)+NaN;
    ext=zeros(4,3)+NaN;
    aaod=NaN(1,4);
    aod=zeros(1,4)+NaN;
    sunerr=NaN;
    skyerr=NaN;
    rfre=zeros(4,2)+NaN;
    dv=zeros(1,22)+NaN;
    reff=zeros(1,3)+NaN;
    vol=zeros(1,3)+NaN;
    rmed=zeros(1,3)+NaN;
    rstd=zeros(1,3)+NaN;
    sphere=NaN;
    flxdn=zeros(1,4)+NaN;
    flxup=zeros(1,4)+NaN;
    albedo=NaN(1,4);
    iw=0;
    jw=0;
    kw=0;
%========================================================    
    file=fname(id).name;
    fid=fopen([fpath file],'r');
    while (feof(fid)==0);
        tline=fgetl(fid);
%         if(isempty(findstr(tline,'Residual after'))==0 & length(tline) > 30);
%             disp(tline);
%             skyerr=str2num(tline(1:12));
%         end;
        
        if(strcmp(tline,'   corse )'));
            iw=iw+1;
%             disp(tline);
            tline=fgetl(fid);
            tmp=str2num(tline);
            ext(iw,1)=tmp(1);
            ext(iw,2)=tmp(2);
            ext(iw,3)=tmp(3);
            ssa(iw,1)=tmp(4);
            ssa(iw,2)=tmp(5);
            ssa(iw,3)=tmp(6);
        end
        
        if(isempty(findstr(tline,' Phase function: ( angle       total       fine      corse )'))==0 &length(tline) >30);
%             disp(tline);
            jw=jw+1;
            for i=1:83;
                tline=fgetl(fid);
                tmp=str2num(tline);
                ang(i)=tmp(1);
                phase(i,jw,1)=tmp(2);
                phase(i,jw,2)=tmp(3);
                phase(i,jw,3)=tmp(4);
            end;
        end;
        
        if(isempty(findstr(tline,'Sun error'))==0 & length(tline) > 20);
            sunerr=str2num(tline(12:end));
%             disp(tline);
        end;
        
       
        if(isempty(findstr(tline,'Sky error'))==0 & length(tline) > 20);
            skyerr=str2num(tline(12:25));
%             disp(tline);
        end;
        
        if(isempty(findstr(tline,'r           min          max          er           eb           erb'))==0 & length(tline) > 30);
%             disp(tline);
            for i=1:4;
                tline=fgetl(fid);
                tmp=str2num(tline);
                rfre(i,1)=tmp(2);
                rfre(i,2)=tmp(8);
            end
        end
        
        if(isempty(findstr(tline,'Radius(micron)             psd'))==0 & length(tline) > 30);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            fgetl(fid);
            for i=1:22;
                tline=fgetl(fid);
                tmp=str2num(tline(1:37));
                r(i)=tmp(1);
                dv(i)=tmp(2);
            end;
        end;
        
        if(isempty(findstr(tline,'Aerosol extinction optical'))==0 & length(tline) > 30);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            fgetl(fid);
            for i=1:4;
                tline=fgetl(fid);
                tmp=str2num(tline);
                aod(i)=tmp(2);
            end
        end
        if(isempty(findstr(tline,'Aerosol absorption optical depth'))==0 & length(tline) > 25);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            fgetl(fid);
            fgetl(fid);
            for i=1:4;
                tline=fgetl(fid);
                tmp=str2num(tline(9:19));
                aaod(i)=tmp;
            end
            disp(aaod);
         end
       

        if(isempty(findstr(tline,'Effective Radius'))==0 & length(tline) > 15);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            for i=1:3;
                tline=fgetl(fid);
                tmp=str2num(tline(9:end));
                reff(i)=tmp;
            end
        end
        
        if(isempty(findstr(tline,'Volume Median'))==0 & length(tline) > 15);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            for i=1:3;
                tline=fgetl(fid);
                tmp=str2num(tline(9:end));
                rmed(i)=tmp;
            end
        end
        
        if(isempty(findstr(tline,'Standard Deviation'))==0 & length(tline) > 15);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            for i=1:3;
                tline=fgetl(fid);
                tmp=str2num(tline(9:end));
                rstd(i)=tmp;
            end
        end
   
        if(isempty(findstr(tline,'Volume concentration'))==0 & length(tline) > 15);
%             disp(tline);
            fgetl(fid);
            fgetl(fid);
            for i=1:3;
                tline=fgetl(fid);
                tmp=str2num(tline(9:end));
                vol(i)=tmp;
            end
        end        
        
        
        if(isempty(findstr(tline,'Sphericity Parameter'))==0 & length(tline) > 15);
%             disp(tline);
            fgetl(fid);
            tline=fgetl(fid);
            tmp=str2num(tline);
            sphere=tmp(1);
        end        
        
        if(isempty(findstr(tline,'(km)  (W/m2)   (W/m2)'))==0 & length(tline) > 15);
%             disp(tline);
            tline=fgetl(fid);
            tmp=str2num(tline);
            flxdn(1)=tmp(2);
            flxup(1)=tmp(3);

            tline=fgetl(fid);
            tmp=str2num(tline);
            flxdn(2)=tmp(2);
            flxup(2)=tmp(3);
            
            tline=fgetl(fid);
            albedo(1)=str2double(tline(3:9));
            albedo(2)=str2double(tline(12:18));
            albedo(3)=str2double(tline(21:27));
            albedo(4)=str2double(tline(30:36));
            disp(albedo);
            pause(10);
            for i=1:2;
                fgetl(fid);
            end;
            
            tline=fgetl(fid);
            tmp=str2num(tline);
            flxdn(3)=tmp(2);
            flxdn(4)=tmp(3);
            
            tline=fgetl(fid);
            tmp=str2num(tline);
            flxup(3)=tmp(2);
            flxup(4)=tmp(3);
            
        end        
    end; %do while
    fclose(fid);
    if(nansum(aod)>0 & sunerr <=0.016 & skyerr <= 15 ); % sun err and sky err should meet specified requirements
        if(aod(1) < 0.40);   %通常是0.40,以上才认为是可靠数据。
            ssa=zeros(4,3)+NaN;
            rfre=zeros(4,2)+NaN;
        end;
        yy=str2num(file(end-16:end-15))+2000;
        mm=str2num(file(end-14:end-13));
        dd=str2num(file(end-12:end-11));
        hh=str2num(file(end-9:end-8));
        mn=str2num(file(end-7:end-6));
        ss=str2num(file(end-5:end-4));
        fprintf(fidw,'%4i,%4i,%4i,%4i,%4i,%4i,',yy,mm,dd,hh,mn,ss);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',aod);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ext(:,1));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ext(:,2));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ext(:,3));

        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ssa(:,1));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ssa(:,2));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',ssa(:,3));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',aaod);

        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',rfre(:,1));
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',rfre(:,2));

        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,',dv);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,',reff);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,',vol);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,',rmed);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,',rstd);

        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',flxdn); 
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',flxup);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,%10.4f,',albedo);
        fprintf(fidw,'%10.4f,%10.4f,%10.4f,',sphere,sunerr,skyerr);
        fprintf(fidw,'\n');
        if(aod(1) < 0.40); %通常是0.40,以上才认为是可靠数据。
            ssa=zeros(4,3)+NaN;
            rfre=zeros(4,2)+NaN;        
        end;
        daily_inversion=[yy,mm,dd,hh,mn,ss,aod,ext(:,1)',ext(:,2)',ext(:,3)',ssa(:,1)',ssa(:,2)',ssa(:,3)',rfre(:,1)',rfre(:,2)',dv,reff,vol,rmed,rstd,flxdn,flxup,sphere,sunerr,skyerr];
        stats_inversion=[stats_inversion;daily_inversion];
    end;
end;
fclose(fidw);
%%=========================================================================
%%     ssa月平均
% wv=[0.44,0.67,0.87,1.02];
% ind0=find(stats_inversion(:,1)>=YearInCount & stats_inversion(:,7) >= 0.4 & stats_inversion(:,26) > 0.63); %AOD at 440 > 0.4 and SSA 675 > 0.63
% % ind0=find(stats_inversion(:,7) >= 0.4 & stats_inversion(:,26) > 0.63); %AOD at 440 > 0.4 and SSA 675 > 0.63
% clev6=stats_inversion(ind0,:);
% clev6(:,3)=1;
% clev6(:,4:6)=0;
% tt=unique(datenum(clev6(:,1:6)));
% for i=1:length(tt);
%     ind=find(datenum(clev6(:,1:6))==tt(i));
%     cmonth(i,1:6)=datevec(tt(i));
%   for j=7:length(clev6(1,:)); 
%     cmonth(i,j)=nanmean(clev6(ind,j));
%   end; 
% end;
% clear ind;
% subplot(3,1,2);
% linec=['-c';'-m';'-r';'-g';'-b';'-k';'-y'];
% ind=find(cmonth(:,1)>=YearInCount);
% plot(datenum(cmonth(ind,1:6)),cmonth(ind,23:26),'-*','markersize',4);hold on;
% datetick('x',2);hold on;
% set(gca,'xtick',[datenum(2012,1,1) datenum(2012,4,1) datenum(2012,7,1) datenum(2012,10,1) datenum(2013,1,1) datenum(2013,4,1)] ,...
%     'xticklabe',{'01/01/12','04/01/12','07/01/12','10/01/12','01/01/13','04/01/13'});
% %set(gca,'xtick',[datenum(2012,1,1) datenum(2012,2,1) datenum(2012,3,1) datenum(2012,4,1)],'xticklabe',{'01/01/12','02/01/12','03/01/12','4/01/12'});
%     
% set(gca,'xlim',[datenum(cmonth(1,1:6))-5 datenum(cmonth(end,1:6))+5]);
% set(gca,'xminortick','on','yminortick','on');
% grid on;
% legend({num2str(wv')},'FontSize',5,'Orientation','horizontal','Location','South');
% set(gca,'ylim',[0.7 1]);
% xlabel('mm/dd/yy');ylabel('SSA');
% title(stns_fn)
% eval(['print -dtiff ' fout stns_fn '_ssa_mon.tif']);
% close;
% 
% 
% %画每天的尺度谱、SSA
% wv=[0.44,0.67,0.87,1.02];
%  for id=datenum(stats_inversion(1,1:3)):datenum(stats_inversion(end,1:3));
%      ind=find(datenum(stats_inversion(:,1:3))==id);
%      if(isempty(ind)==0);
%          subplot(2,1,1);
%          semilogx(r',stats_inversion(ind,43:64),'-*');
%          legend([num2str(stats_inversion(ind,7),'%6.2f')]);
%          set(gca,'xlim',[0.04 20],'xminortick','on','yminortick','on');
%          set(gca,'xtick',[0.05 0.1 0.2 0.5 1.0 2.0 5.0 10.0],'xticklabel',{'0.05','0.1','0.2','0.5','1.0','2.0','5.0','10.0'});
% %         
%          yyyy=num2str(stats_inversion(ind(1),1));
%          if(stats_inversion(ind(1),2)>=10);
%              mm=num2str(stats_inversion(ind(1),2));
%          else
%              mm=['0' num2str(stats_inversion(ind(1),2))];
%          end;
%          if(stats_inversion(ind(1),3)>=10);
%             dd=num2str(stats_inversion(ind(1),3));
%          else
%              dd=['0' num2str(stats_inversion(ind(1),3))];
%          end;
% %         
%          ind=find(datenum(stats_inversion(:,1:3))==id & stats_inversion(:,7) >=0.4);
%          if(isempty(ind)==0);
%              subplot(2,1,2);
%              plot(wv,stats_inversion(ind,23:26));
%              set(gca,'xlim',[0.42 1.05]);
%              legend([num2str(stats_inversion(ind,7),'%6.2f')]);
%          end;
% % 
%          eval(['print -dtiff ' fout 'daily_inversion_' yyyy mm dd '.tif']);
%          close;
% %         
%      end;
%  end;
% 
% 
% %=========================================================================
% %     绘图
% %ssa
% figure;
% subplot(4,1,4);
% ind=find(stats_inversion(:,1)>=YearInCount & stats_inversion(:,7) >= 0.4 & stats_inversion(:,26) > 0.63); %AOD at 440 > 0.4 and SSA 675 > 0.63
% plot(datenum(stats_inversion(ind,1:6)),stats_inversion(ind,23:26),'-*');hold on;
% for im=1:12;
%     plot([datenum(YearInCount,im,30),datenum(YearInCount,im,30)],[0.6 1],'-r');
% end;
% datetick('x',12);hold on;
% set(gca,'xlim',[datenum(stats_inversion(ind(1),1:6))-5 datenum(stats_inversion(end,1:6))+5]);
% set(gca,'xtick',[datenum(YearInCount,1,1) datenum(YearInCount,2,1) datenum(YearInCount,4,1) datenum(YearInCount,6,1) datenum(YearInCount,8,1) datenum(YearInCount,10,1) datenum(YearInCount,12,1)],...
%     'xticklabe',{['Jan',num2str(mod(YearInCount,100))],...
%     ['Feb',num2str(mod(YearInCount,100))],['Apr',num2str(mod(YearInCount,100))],...
%     ['Jun',num2str(mod(YearInCount,100))],['Aug',num2str(mod(YearInCount,100))]',...
%     ['Oct',num2str(mod(YearInCount,100))],['Dec',num2str(mod(YearInCount,100))]});
% set(gca,'xminortick','on','yminortick','on');
% legend(num2str(wv'),'Orientation','horizontal','Location','South');
% set(gca,'ylim',[0.6 1]);
% xlabel('Date (MMMYY)');ylabel('SSA');
% title(stns_fn)
% eval(['print -dtiff ' fout stns_fn '_stats_ssa.tif']);
% close;
% 
% %size volume
% figure;
% y=[0.05	0.066	0.086	0.113	0.148	0.194	0.255	0.335	0.439	0.576	0.756	0.992	1.302	1.708	2.241	2.94	3.857	5.051	6.641	8.713	11.43	15];
% ind=find(stats_inversion(:,1)==YearInCount);
% stats_inversion=stats_inversion(ind,:);
% for id=1:length(stats_inversion(:,1));
%     x=zeros(1,length(y))+datenum(stats_inversion(id,1:6));
%     z=stats_inversion(id,43:64);
%     plot3(x,y,z,'-b');hold on;
% end;
% set(gca,'Yscale','Log','xminortick','on','yminortick','on','zminortick','on');
% set(gca,'ylim',[0.025 25],'zlim',[0 0.5]);
% datetick('x',12);hold on;
% zlabel('Volume');
% text(datenum(stats_inversion(1,1:6)),25,0.45,stns_fn);
% eval(['print -dtiff ' fout stns_fn '_stats_volume.tif']);
% close;




% pval=prctile(stats_inversion(:,7),[0,25,50,75,100]);
% for i=1:4;
%     subplot(2,2,i);
%     ind=find(stats_inversion(:,7) >=pval(i) & stats_inversion(:,7) <= pval(i+1));
%     semilogx(r',nanmedian(stats_inversion(ind,43:64),1),'-b*');
%     set(gca,'xlim',[0.05 15],'xminortick','on','yminortick','on');
%     set(gca,'xtick',[0.05 0.1 0.2 0.5 1.0 2.0 5.0 10.0],'xticklabel',{'0.05','0.1','0.2','0.5','1.0','2.0','5.0','10.0'});
%     grid on;
%     tmp=nanmean(stats_inversion(ind,7));
%     title(['AOD_4_4_0_n_m = ' num2str(tmp,'%6.2f')]);
%     xlabel('Raidus (\mum)');
%     ylabel('Volumn concentration (\mum^-^3/\mum^-^2)');
% end;
% eval(['print -depsc ' fout 'stats_size_distribution.eps']);
% close;

% %=========================================== 
% for id=datenum(2008,1,23):datenum(2009,6,1);
%     ind=find(datenum(stats_inversion(:,1:3))== id & stats_inversion(:,7) >=0.4 );
%     if(length(ind)>0);
%         subplot(2,2,1);
%         plot(wv',stats_inversion(ind,7:10)','-*');
%         set(gca,'xlim',[0.4 1.1],'xminortick','on','yminortick','on');
%         subplot(2,2,2);
%         plot(wv',stats_inversion(ind,23:26)','-*');
%         set(gca,'xlim',[0.4 1.1],'xminortick','on','yminortick','on');
%         subplot(2,2,3);
%         plot(wv',stats_inversion(ind,39:42)','-*');
%         set(gca,'xlim',[0.4 1.1],'xminortick','on','yminortick','on');
%         subplot(2,2,4);
%         semilogx(r,stats_inversion(ind,43:64));
%         set(gca,'xlim',[0.04 15.5],'xminortick','on','yminortick','on');
%         yy=stats_inversion(ind(1),1);
%         mm=stats_inversion(ind(1),2);
%         dd=stats_inversion(ind(1),3);
%         eval(['print -dtiff D:\Research_result\Ulumqi_cimel\fig_aeronet_inversion_' num2str(yy) strmm(mm,:) strdd(dd,:) '.tiff']);
%         close;
%     end;
% end;
% %==============================================
;Make 360 degree image of the Sun with AIA and STEREO data mounted locally at LMSAL
;WAVELENGTH set the EUV wavelength to use for the map (if it is an array of 3 the elements will apply to: [aia, sta, stb])
;			171, 193/195
;RANGESECSTAB = range to search for stereo data to match AIA
;REBIN = Resolution to rebin the data to. [x size, y size]
;RESOLUTION = degrees per pixel in the final carrington map
;NOAEC = searches for aia data with AECTYPE keyword equal to 0 or 1, meaning no auto exposure control. 
;			Uses RANGESECSTAB keyword for search time range. Finds nearest file to input DATE.

function make_euv_360sun, date,wavelength=inwavelength, $
	logimg=logimg, gaussweight=gaussweight, sigweight=sigweight, hgsigweight=hgsigweight, bytescale=bytescale, $
	resolution=inresolution, rebin=rebin, nomatchscale=nomatchscale, $
	aiamap=inaiamap, stamap=instamap, stbmap=instbmap, $
	faia=faia, fsta=fsta, fstb=fstb, _extra=_extra, rangesecstab=inrangesecstab, noaec=noaec, $
	pcincrsun=inpcrsun, skipqualcheck=skipqualcheck, synoptic=synoptic, outpath_synoptic=outpath

if n_elements(inwavelength) ne 1 then wavelength=171 else wavelength=inwavelength

if n_elements(inrangesecstab) ne 1 then rangesecstab=3600 else rangesecstab=inrangesecstab

date=anytim(date,/vms)
tim=anytim(date)

if n_elements(inwavelength) eq 1 then begin
	wavelengthaia=wavelength
	wavelengthsta=wavelength
	wavelengthstb=wavelength
endif else begin
	wavelengthaia=inwavelength[0]
	wavelengthsta=inwavelength[1]
	wavelengthstb=inwavelength[2]
endelse


;dowload an AIA map----------------------------------------------------------->
if n_elements(inaiamap) eq 1 then aiamap=inaiamap else begin
	dateendaia=anytim(tim+rangesecstab,/vms)

	if keyword_set(synoptic) then begin

stop

		aiafiles=getjsoc_synoptic_read(date, dateendaia, wavelength=wavelengthaia, outpath=outpath, info_struct=info_struct, skipqualcheck=skipqualcheck, /nodata)
		aiaabst=abs(anytim(file2time(aiafiles))-tim)
		faia=(aiafiles)[where(aiaabst eq min(aiaabst))]		
		aiamap=getjsoc_synoptic_read(date, remfilelist=faia, wavelength=wavelengthaia, outpath=outpath, info_struct=info_struct, skipqualcheck=skipqualcheck)
		
	endif else begin
		if keyword_set(noaec) then begin
			dum=getjsoc_sdo_read(date, dateendaia, filelist=faia, /getaia, wavelength=wavelengthaia, /nodat, _extra=_extra, outindex=aiaindexarr, noaec=noaec, skipqualcheck=skipqualcheck)
			aiaabst=abs(anytim(aiaindexarr.date_obs)-tim)
			aiabestdate=(aiaindexarr.date_obs)[where(aiaabst eq min(aiaabst))]
			aiamap=getjsoc_sdo_read(anytim(aiabestdate,/vms), filelist=faia, /getaia, wavelength=wavelengthaia, /calibrate, rebin=rebin, _extra=_extra, /normalize, noaec=noaec, skipqualcheck=skipqualcheck)
	
		endif else aiamap=getjsoc_sdo_read(date, filelist=faia, /getaia, wavelength=wavelengthaia, /calibrate, rebin=rebin, _extra=_extra, /normalize, skipqualcheck=skipqualcheck)
	endelse
endelse

if data_type(aiamap) ne 8 then begin
	print,'NO AIA DATA FOUND! Wave = '+strtrim(wavelengthaia,2)
	print,'data_type(aiamap) ne 8'
	
	return, -1
endif else print,'GOT AIA MAP'

;download stereo A map-------------------------------------------------------->
if n_elements(instamap) eq 1 then stamap=instamap else begin
	if n_elements(fsta) ne 1 then begin
;		stafiles=get_euvi_read(date, /a_sc, wavelength=wavelengthsta,/nodata, rangesec=rangesecstab, index=staindex)
		stafiles=get_euvi_read(date, /a_sc, wavelength=wavelengthsta, rangesec=rangesecstab, /noindex)
        statims=anytim(file2time(stafiles))        
        
;        if data_type(staindex) ne 8 or stafiles[0] eq '' then goto,no_sta_data
		if stafiles[0] eq '' then goto,no_sta_data

		;statims=anytim(staindex.date_obs)
		wstamin=(where(abs(statims-tim) eq min(abs(statims-tim))))[0]
		stafile=stafiles[wstamin]
		fsta=stafile
	endif	
	stamap=get_euvi_read(date, /a_sc, wavelength=wavelengthsta, outsize=rebin, filelist=fsta, /calibrate)
endelse

no_sta_data:
if data_type(stamap) ne 8 then begin
	print,'NO STEREO A DATA FOUND! Wave = '+strtrim(wavelengthsta,2)
	print,'data_type(stamap) ne 8'
		
;stop
	
	return, -1
endif else print,'GOT STA MAP'

;download stereo B map-------------------------------------------------------->
if n_elements(instbmap) eq 1 then stbmap=instbmap else begin
	if n_elements(fstb) ne 1 then begin
;		stbfiles=get_euvi_read(date, /b_sc, wavelength=wavelengthstb,/nodata, rangesec=rangesecstab, index=stbindex)
		stbfiles=get_euvi_read(date, /b_sc, wavelength=wavelengthstb, rangesec=rangesecstab, /noindex)
        stbtims=anytim(file2time(stbfiles)) 
        
;        if data_type(stbindex) ne 8 or stbfiles[0] eq '' then goto,no_stb_data
		if stbfiles[0] eq '' then goto,no_stb_data
	
		;stbtims=anytim(stbindex.date_obs)
		wstbmin=(where(abs(stbtims-tim) eq min(abs(stbtims-tim))))[0]
		stbfile=stbfiles[wstbmin]
		fstb=stbfile
	endif	
	stbmap=get_euvi_read(date, /b_sc, wavelength=wavelengthstb, outsize=rebin, filelist=fstb, /calibrate)
endelse

no_stb_data:
if data_type(stbmap) ne 8 then begin
	print,'NO STEREO B DATA FOUND! Wave = '+strtrim(wavelengthstb,2)
	print,'data_type(stbmap) ne 8'
		
;stop
	
	return, -1
endif else print,'GOT STB MAP'

;HACK change radius of Solar limb
;Check WCS_RSUN to make sure it matches
if n_elements(inpcrsun) eq 1 then begin
   pcinc=inpcrsun
   aiamap.rsun=aiamap.rsun+pcinc*aiamap.rsun
   stamap.rsun=stamap.rsun+pcinc*stamap.rsun
   stbmap.rsun=stbmap.rsun+pcinc*stbmap.rsun
endif

;Make all maps floating point
add_prop,aiamap,data=float(aiamap.data),/replace
add_prop,stamap,data=float(stamap.data),/replace
add_prop,stbmap,data=float(stbmap.data),/replace

;stop

;Make NaN's into Zeros
waianan=where(finite(aiamap.data) ne 1)
wstanan=where(finite(stamap.data) ne 1)
wstbnan=where(finite(stbmap.data) ne 1)
if (waianan)[0] ne -1 then begin & aiafin=aiamap.data & aiafin[waianan]=1d-9 & add_prop,aiamap,data=aiafin,/replace & endif
if (wstanan)[0] ne -1 then begin & stafin=stamap.data & stafin[wstanan]=1d-9 & add_prop,stamap,data=stafin,/replace & endif
if (wstbnan)[0] ne -1 then begin & stbfin=stbmap.data & stbfin[wstbnan]=1d-9 & add_prop,stbmap,data=stbfin,/replace & endif

;Use STB resolution since it has largest pixels on SUN and determine how to deproject to 
;lat vs lon, so as not to lose info on solar surface.

latbin=stbmap.dx*360./2./!pi/stbmap.rsun
lonbin=latbin
lonbound=[-180,179.9]
latbound=[-90,90]

;AIA Find map of angle to LOS------------------------------------------------->
aialos=map_degtolos(aiamap, offlimb=aiaoff)
aialos=90.-aialos/!dtor
if keyword_set(gaussweight) then aialos=1-exp(-aialos^2./30^2.)
if keyword_set(sigweight) then aialos=1./(1+exp(((90-aialos)-70.)*0.7))

;STA Find map of angle to LOS------------------------------------------------->
stalos=map_degtolos(stamap, offlimb=staoff)
stalos=90.-stalos/!dtor
if keyword_set(gaussweight) then stalos=1-exp(-stalos^2./30^2.)
if keyword_set(sigweight) then stalos=1./(1+exp(((90-stalos)-70.)*0.7))

;STB Find map of angle to LOS------------------------------------------------->
stblos=map_degtolos(stbmap, offlimb=stboff)
stblos=90.-stblos/!dtor
if keyword_set(gaussweight) then stblos=1-exp(-stblos^2./30^2.)
if keyword_set(sigweight) then stblos=1./(1+exp(((90-stblos)-70.)*0.7))

;stop

if keyword_set(hgsigweight) then begin
	hgsigfalloff=5.

;Determine the HG coordinates for each map------------------------------------>
	map_gethg, aiamap, wcs=aiamap.wcs, hglon=aiahglon, hglat=aiahglat
	map_gethg, stamap, wcs=stamap.wcs, hglon=stahglon, hglat=stahglat
	map_gethg, stbmap, wcs=stbmap.wcs, hglon=stbhglon, hglat=stbhglat

;Determine HG position half-way between the STA or STB and AIA center of FOVs
	stahgpos=(stamap.index.hgln_obs)
	stbhgpos=(stbmap.index.hgln_obs)
	aiabnd=[abs(stbhgpos)/2., abs(stahgpos)/2.]
	stabnd=[abs(stahgpos)/2., (360.-abs(stahgpos)-abs(stbhgpos))/2.]
	stbbnd=[(360.-abs(stahgpos)-abs(stbhgpos))/2., abs(stbhgpos)/2.]

;Shift the STA,B HG lon maps to be SC-centered
	stahglon=stahglon-stahgpos
	stbhglon=stbhglon-stbhgpos

;Make AIA LOS map
	aialose=1./(1+exp(((abs(aiahglon))-aiabnd[0])*hgsigfalloff))
	aialosw=1./(1+exp(((abs(aiahglon))-aiabnd[1])*hgsigfalloff))
	aialose[where(aiahglon ge 0)]=0.
	aialosw[where(aiahglon lt 0)]=0.
	aialos=aialose+aialosw

;Make STA LOS map
	stalose=1./(1+exp((abs(stahglon)-stabnd[0])*hgsigfalloff))
	stalosw=1./(1+exp((abs(stahglon)-stabnd[1])*hgsigfalloff))
	stalose[where(stahglon ge 0)]=0.
	stalosw[where(stahglon lt 0)]=0.
	stalos=stalose+stalosw
	
;Make STB LOS map
	stblose=1./(1+exp(((abs(stbhglon))-stbbnd[0])*hgsigfalloff))
	stblosw=1./(1+exp(((abs(stbhglon))-stbbnd[1])*hgsigfalloff))
	stblose[where(stbhglon ge 0)]=0.
	stblosw[where(stbhglon lt 0)]=0.
	stblos=stblose+stblosw

endif

;stop

;Filter LOS maps for NaNs. and read them into maps---------------------------->
aialos[where(aiaoff eq 0)]=0.
aialosmap=aiamap
aialosmap.data=aialos

stalos[where(staoff eq 0)]=0.
stalosmap=stamap
stalosmap.data=stalos

stblos[where(stboff eq 0)]=0.
stblosmap=stbmap
stblosmap.data=stblos

;stop

if not keyword_set(nomatchscale) then begin

;Scale histogram maximums to line up
;Fit a poisson function to determine the peak locations
	yhistaia=histogram(aiamap.data[where(aiaoff eq 1 and finite(aiamap.data) eq 1)],loc=xhistaia)
	testymaxaia=double(max(yhistaia))
	testxmaxaia=double(xhistaia[(where(yhistaia eq testymaxaia))[0]])
	ppaia=mpfitexpr('P[0]*(exp(-X)*(exp(1d)*X)^P[1])/((P[1])^(P[1]))',double(xhistaia),double(yhistaia),err,[testymaxaia,testxmaxaia])
;	ppaia=mpfitexpr('P[0]*poisson(X,P[1])',double(xhistaia),double(yhistaia),err,[testymaxaia,testxmaxaia])
	maxvalaia=ppaia[1]

	yhiststa=histogram(stamap.data[where(staoff eq 1 and finite(stamap.data) eq 1)],loc=xhiststa)
	testymaxsta=double(max(yhiststa))
	testxmaxsta=double(xhiststa[(where(yhiststa eq testymaxsta))[0]])
	ppsta=mpfitexpr('P[0]*(exp(-X)*(exp(1d)*X)^P[1])/((P[1])^(P[1]))',double(xhiststa),double(yhiststa),err,[testymaxsta,testxmaxsta])
;	ppsta=mpfitexpr('P[0]*poisson(X,P[1])',double(xhiststa),double(yhiststa),err,[testymaxsta,testxmaxsta])
	maxvalsta=ppsta[1]
	
	yhiststb=histogram(stbmap.data[where(stboff eq 1 and finite(stbmap.data) eq 1)],loc=xhiststb)
	testymaxstb=double(max(yhiststb))
	testxmaxstb=double(xhiststb[(where(yhiststb eq testymaxstb))[0]])
	ppstb=mpfitexpr('P[0]*(exp(-X)*(exp(1d)*X)^P[1])/((P[1])^(P[1]))',double(xhiststb),double(yhiststb),err,[testymaxstb,testxmaxstb])
;	ppstb=mpfitexpr('P[0]*poisson(X,P[1])',double(xhiststb),double(yhiststb),err,[testymaxstb,testxmaxstb])
	maxvalstb=ppstb[1]

;multiply scaling factor
	stamap.data=stamap.data*(maxvalaia/maxvalsta)
	stbmap.data=stbmap.data*(maxvalaia/maxvalstb)

endif

;raiamapdat=rebin(aiamap.data,2048,2048)
;raiaoff=rebin(aiaoff,2048,2048)

;plot_hist,aiamap.data[where(aiaoff eq 1)],bin=10,xran=[0,5000],/log
;plot_hist,stamap.data[where(staoff eq 1)],bin=10,xran=[0,5000],/log,/oplot
;plot_hist,stbmap.data[where(stboff eq 1)],bin=10,xran=[0,5000],/log,/oplot
;plot_hist,raiamapdat[where(raiaoff eq 1)],bin=2,xran=[0,1000],/log,/oplot

;stop



;Remap AIA image-------------------------------------------------------------->
aialatlon=make_latlon_img(dum, map=aiamap, tmid=aiamap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=aiaprojmask, outhg=aiahgcoord, $
        auxmap=aialosmap, outaux=aialoslatlon)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter
        ;outavgimg=avgimg, outfluxmap=outfluxmap, noremap=noremap
;aiaprojmask[where(aialatlon eq 1)]=0

;SHOULD I BE DIFFERENTIALLY ROTATING ALL MAPS TO A SINGLE TIME??? I think yes.....



;Remap STA image-------------------------------------------------------------->
stalatlon=make_latlon_img(dum, map=stamap, tmid=stamap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=staprojmask, outhg=stahgcoord, $
        auxmap=stalosmap, outaux=staloslatlon)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter
        ;outfluxmap=outfluxmap, outavgimg=avgimg, noremap=noremap
;staprojmask[where(stalatlon eq 0)]=0

;Find map of angle to LOS----------------------------------------------------->
;stblos=map_degtolos(stbmap, offlimb=stboff)
;stblos=90.-stblos/!dtor
;stblos[where(stboff eq 0)]=0
;stblosmap=stbmap
;stblosmap.data=stblos

;Remap STB image-------------------------------------------------------------->
stblatlon=make_latlon_img(dum, map=stbmap, tmid=stbmap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=stbprojmask, outhg=stbhgcoord, $
        auxmap=stblosmap, outaux=stbloslatlon)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter, $
        ;outfluxmap=outfluxmap, outavgimg=avgimg, noremap=noremap
;stbprojmask[where(stblatlon eq 0)]=0

;Weight lat lon map pixels by their distance to disk center------------------->

;plot_image, (aialoslatlon+staloslatlon+stbloslatlon)/3.

;Filter for NaNs
waialosnan=where(finite(aialoslatlon) ne 1)
wstalosnan=where(finite(staloslatlon) ne 1)
wstblosnan=where(finite(stbloslatlon) ne 1)
if waialosnan[0] ne -1 then aialoslatlon[waialosnan]=0.
if wstalosnan[0] ne -1 then staloslatlon[wstalosnan]=0.
if wstblosnan[0] ne -1 then stbloslatlon[wstblosnan]=0.

;Maps are weighted by pixel distance from disk center
if keyword_set(logimg) then euvmap=lognonan(aialatlon)*aialoslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+ $
                                   lognonan(stalatlon)*staloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+ $
                                   lognonan(stblatlon)*stbloslatlon/(aialoslatlon+staloslatlon+stbloslatlon) $
	else euvmap=(aialatlon)*aialoslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+ $
                    (stalatlon)*staloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+ $
                    (stblatlon)*stbloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)

;Make a map structure
map2index,stbmap,euvind
index2map,euvind,euvmap,mapeuv
add_prop,mapeuv,dx=lonbin,/replace
add_prop,mapeuv,dy=latbin,/replace
add_prop,mapeuv,xc=0.0,/replace
add_prop,mapeuv,yc=0.0,/replace
add_prop,mapeuv,time=date,/replace
add_prop,mapeuv,roll_angle=0.0,/replace
add_prop,mapeuv,roll_center=[0.0,0.0],/replace
add_prop,mapeuv,rsun=aiamap.rsun,/replace
add_prop,mapeuv,aiatime=aiamap.time
add_prop,mapeuv,statime=stamap.time
add_prop,mapeuv,stbtime=stbmap.time
add_prop,mapeuv,xunits='degrees',/replace
add_prop,mapeuv,yunits='degrees',/replace
add_prop,mapeuv,aiawave=wavelengthaia
add_prop,mapeuv,stawave=wavelengthsta
add_prop,mapeuv,stbwave=wavelengthstb
add_prop,mapeuv,aiaaectype=aiamap.index.aectype
add_prop,mapeuv,aiaexptime=aiamap.index.exptime
add_prop,mapeuv,staexptime=stamap.index.exptime
add_prop,mapeuv,stbexptime=stbmap.index.exptime
add_prop,mapeuv,aiaqual=aiamap.index.quallev0
add_prop,mapeuv,staqual='?'
add_prop,mapeuv,stbqual='?'

;include original indexes and WCS structures of FITs files
add_prop,mapeuv,aiaindex=aiamap.index
add_prop,mapeuv,staindex=stamap.index
add_prop,mapeuv,stbindex=stbmap.index
add_prop,mapeuv,aiawcs=aiamap.wcs
add_prop,mapeuv,stawcs=stamap.wcs
add_prop,mapeuv,stbwcs=stbmap.wcs

;stop

return, mapeuv

end

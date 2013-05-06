;Make 360 degree image of the Sun with AIA and STEREO data mounted locally at LMSAL
;WAVELENGTH set the EUV wavelength to use for the map (if it is an array of 3 the elements will apply to: [aia, sta, stb])
;			171, 193/195
;RANGESECSTAB = range to search for stereo data to match AIA
;REBIN = Resolution to rebin the data to. [x size, y size]
;RESOLUTION = degrees per pixel in the final carrington map
;NOAEC = searches for aia data with AECTYPE keyword equal to 0 or 1, meaning no auto exposure control. 
;			Uses RANGESECSTAB keyword for search time range. Finds nearest file to input DATE.

function make_euv_360sun, date,wavelength=inwavelength, $
	logimg=logimg, bytescale=bytescale, resolution=inresolution, rebin=rebin, $
	aiamap=inaiamap, stamap=instamap, stbmap=instbmap, $
	faia=faia, fsta=fsta, fstb=fstb, _extra=_extra, rangesecstab=inrangesecstab, noaec=noaec

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
	if keyword_set(noaec) then begin
		dateendaia=anytim(tim+rangesecstab,/vms)
		dum=getjsoc_sdo_read(date, dateendaia, filelist=faia, /getaia, wavelength=wavelengthaia, /nodat, _extra=_extra, outindex=aiaindexarr, noaec=noaec)
		aiaabst=abs(anytim(aiaindexarr.date_obs)-tim)
		aiabestdate=(aiaindexarr.date_obs)[where(aiaabst eq min(aiaabst))]
		aiamap=getjsoc_sdo_read(aiabestdate, filelist=faia, /getaia, wavelength=wavelengthaia, /calibrate, rebin=rebin, _extra=_extra, /normalize, noaec=noaec)

	endif else aiamap=getjsoc_sdo_read(date, filelist=faia, /getaia, wavelength=wavelengthaia, /calibrate, rebin=rebin, _extra=_extra, /normalize)

endelse

if data_type(aiamap) ne 8 then begin
	print,'NO AIA DATA FOUND! Wave = '+strtrim(wavelength,2)
	print,'data_type(aiamap) ne 8'
	
;stop
	
	return, -1
endif

;download stereo A map-------------------------------------------------------->
if n_elements(instamap) eq 1 then stamap=instamap else begin
	if n_elements(fsta) ne 1 then begin
		stafiles=get_euvi_read(date, /a_sc, wavelength=wavelengthsta,/nodata, rangesec=rangesecstab, index=staindex)
	
		statims=anytim(staindex.date_obs)
		wstamin=(where(abs(statims-tim) eq min(abs(statims-tim))))[0]
		stafile=stafiles[wstamin]
		fsta=stafile
	endif	
	stamap=get_euvi_read(date, /a_sc, wavelength=wavelength, /calibrate, outsize=rebin, filelist=fsta)
endelse

if data_type(stamap) ne 8 then begin
	print,'NO STEREO A DATA FOUND! Wave = '+strtrim(wavelength,2)
	print,'data_type(stamap) ne 8'
		
;stop
	
	return, -1
endif

;download stereo B map-------------------------------------------------------->
if n_elements(instbmap) eq 1 then stbmap=instbmap else begin
	if n_elements(fstb) ne 1 then begin
		stbfiles=get_euvi_read(date, /b_sc, wavelength=wavelengthstb,/nodata, rangesec=rangesecstab, index=stbindex)
	
		stbtims=anytim(stbindex.date_obs)
		wstbmin=(where(abs(stbtims-tim) eq min(abs(stbtims-tim))))[0]
		stbfile=stbfiles[wstbmin]
		fstb=stbfile
	endif	
	stbmap=get_euvi_read(date, /b_sc, wavelength=wavelength, /calibrate, outsize=rebin, filelist=fstb)
endelse

if data_type(stamap) ne 8 then begin
	print,'NO STEREO B DATA FOUND! Wave = '+strtrim(wavelength,2)
	print,'data_type(stbmap) ne 8'
		
;stop
	
	return, -1
endif

;Make all maps floating point
add_prop,aiamap,data=float(aiamap.data),/replace
add_prop,stamap,data=float(stamap.data),/replace
add_prop,stbmap,data=float(stbmap.data),/replace

;Use STB resolution since it has largest pixels on SUN and determine how to deproject to 
;lat vs lon, so as not to lose info on solar surface.

latbin=stbmap.dx*360./2./!pi/stbmap.rsun
lonbin=latbin
lonbound=[-180,179.9]
latbound=[-90,90]

;Find map of angle to LOS----------------------------------------------------->
aialos=map_degtolos(aiamap, offlimb=aiaoff)
aialos=90.-aialos/!dtor
aialos[where(aiaoff eq 0)]=0
aialosmap=aiamap
aialosmap.data=aialos

;Find map of angle to LOS----------------------------------------------------->
stalos=map_degtolos(stamap, offlimb=staoff)
stalos=90.-stalos/!dtor
stalos[where(staoff eq 0)]=0
stalosmap=stamap
stalosmap.data=stalos

;Find map of angle to LOS----------------------------------------------------->
stblos=map_degtolos(stbmap, offlimb=stboff)
stblos=90.-stblos/!dtor
stblos[where(stboff eq 0)]=0
stblosmap=stbmap
stblosmap.data=stblos

;Scale histogram maximums to line up
yhistaia=histogram(aiamap.data[where(aiaoff eq 1)],loc=xhistaia)
maxvalaia=xhistaia[(where(yhistaia eq max(yhistaia)))[0]]

yhiststa=histogram(stamap.data[where(staoff eq 1)],loc=xhiststa)
maxvalsta=xhiststa[(where(yhiststa eq max(yhiststa)))[0]]

yhiststb=histogram(stbmap.data[where(stboff eq 1)],loc=xhiststb)
maxvalstb=xhiststb[(where(yhiststb eq max(yhiststb)))[0]]

;multiply scaling factor
stamap.data=stamap.data*(maxvalaia/maxvalsta)
stbmap.data=stbmap.data*(maxvalaia/maxvalstb)

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

;Maps are weighted by pixel distance from disk center
if keyword_set(logimg) then euvmap=lognonan(aialatlon)*aialoslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+lognonan(stalatlon)*staloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+lognonan(stblatlon)*stbloslatlon/(aialoslatlon+staloslatlon+stbloslatlon) $
	else euvmap=(aialatlon)*aialoslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+(stalatlon)*staloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)+(stblatlon)*stbloslatlon/(aialoslatlon+staloslatlon+stbloslatlon)

;Make a map structure
map2index,stbmap,euvind
index2map,euvind,euvmap,mapeuv
add_prop,mapeuv,dx=lonbin,/replace
add_prop,mapeuv,dy=latbin,/replace
add_prop,mapeuv,time=date,/replace
add_prop,mapeuv,aiatime=aiamap.time
add_prop,mapeuv,statime=stamap.time
add_prop,mapeuv,stbtime=stbmap.time
add_prop,mapeuv,xunits='degrees',/replace
add_prop,mapeuv,yunits='degrees',/replace
add_prop,mapeuv,roll_angle=0.0,/replace
add_prop,mapeuv,aiawave=wavelengthaia
add_prop,mapeuv,stawave=wavelengthsta
add_prop,mapeuv,stbwave=wavelengthstb

return, mapeuv

end
;Make 360 degree image of the Sun with AIA and STEREO data mounted locally at LMSAL
;WAVELENGTH set the EUV wavelength to use for the map
;			171, 193/195
;RANGESECSTAB = range to search for stereo data to match AIA
;REBIN = Resolution to rebin the data to. [x size, y size]
;RESOLUTION = degrees per pixel in the final carrington map

pro make_euv_360sun, date,wavelength=inwavelength, $
	bytescale=bytescale, resolution=inresolution, rebin=rebin, $
	aiamap=inaiamap, stamap=instamap, stbmap=instbmap, $
	faia=faia, fsta=fsta, fstb=fstb, _extra=_extra, rangesecstab=inrangesecstab

if n_elements(inwavelength) ne 1 then wavelength=171 else wavelength=inwavelength

if n_elements(inrangesecstab) ne 1 then rangesecstab=3600 else rangesecstab=inrangesecstab

date=anytim(date,/vms)
tim=anytim(date)

;dowload an AIA map----------------------------------------------------------->
if n_elements(inaiamap) eq 1 then aiamap=inaiamap $
	else aiamap=getjsoc_sdo_read(date, filelist=faia, /getaia, wavelength=wavelength, /calibrate, rebin=rebin, _extra=_extra)

if data_type(aiamap) ne 8 then begin
	print,'NO AIA DATA FOUND! Wave = '+strtrim(wavelength,2)
	print,'data_type(aiamap) ne 8'
	
stop
	
	return
endif

;download stereo A map-------------------------------------------------------->
if n_elements(instamap) eq 1 then stamap=instamap else begin
	if n_elements(fsta) ne 1 then begin
		stafiles=get_euvi_read(date, /a_sc, wavelength=wavelength,/nodata, rangesec=rangesecstab, index=staindex)
	
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
		
stop
	
	return
endif

;download stereo B map-------------------------------------------------------->
if n_elements(instbmap) eq 1 then stbmap=instbmap else begin
	if n_elements(fstb) ne 1 then begin
		stbfiles=get_euvi_read(date, /b_sc, wavelength=wavelength,/nodata, rangesec=rangesecstab, index=stbindex)
	
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
		
stop
	
	return
endif

;Use STB resolution since it has largest pixels on SUN and determine how to deproject to 
;lat vs lon, so as not to lose info on solar surface.

latbin=stbmap.dx*360./2./!pi/stbmap.rsun
lonbin=latbin
lonbound=[-180,179.9]
latbound=[-90,90]

aialatlon=make_latlon_img(dum, map=aiamap, tmid=aiamap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=aiaprojmask, outhg=aiahgcoord)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter
        ;outavgimg=avgimg, outfluxmap=outfluxmap, noremap=noremap
aiaprojmask[where(aialatlon eq 1)]=0

;SHOULD I BE DIFFERENTIALLY ROTATING ALL MAPS TO A SINGLE TIME???

stalatlon=make_latlon_img(dum, map=stamap, tmid=stamap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=staprojmask, outhg=stahgcoord)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter
        ;outfluxmap=outfluxmap, outavgimg=avgimg, noremap=noremap
staprojmask[where(stalatlon eq 0)]=0

stblatlon=make_latlon_img(dum, map=stbmap, tmid=stbmap.time, $ ; logscale=logscale, missval=missval, $
        latbin=latbin, lonbin=lonbin, lonbound=lonbound, latbound=latbound, outmask=stbprojmask, outhg=stbhgcoord)
        ;setrebin=setrebin, rebinfact=rebinfact, $ ;dofilter=dofilter, $
        ;outfluxmap=outfluxmap, outavgimg=avgimg, noremap=noremap
stbprojmask[where(stblatlon eq 0)]=0






stop






end
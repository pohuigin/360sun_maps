;Plot a latitude versus longitude image of the Sun, using a map structure from MAKE_EUV_360sun.

pro plot_360sun, map, _extra=_extra, sctimpos=insctimpos,wave=wave, drangescl=indrange, log=log

if n_elements(insctimpos) eq 0 then sctimpos=[0.1,0.1] else sctimpos=insctimpos

if n_elements(indrange) gt 0 then begin
	drange=indrange
	min=drange[0]
	max=drange[1]
	if keyword_set(log) then begin
		min=alog10(min)
		max=alog10(max)
	endif
endif

;stop

mapp=map
if keyword_set(log) then mapp.data=bytscl(alog10(map.data),min=min,max=max) $
	else mapp.data=bytscl((map.data),min=min,max=max)

plot_map, mapp,position=[0.05,0.05,0.95,0.95], _extra=_extra,/iso;,log=log

xyouts,sctimpos[0],sctimpos[1],'STB '+map.stbtime+', AIA '+map.aiatime+', STA '+map.statime, chars=2, /norm

vline,0

hline,0





















end

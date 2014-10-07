SELECT e.SubjectStudyID + 0 AS id, e.PIDN, s1.SubjectStudyID + 0 AS SENAS_ID, 
DATE(p1.DOB) AS DOB, p1.GENDER, DATE(p1.DOD) AS DOD, 
IF(p1.ETHNICITY>=1,p1.ETHNICITY,null) ETHNICITY,
udsdemo.HISPANIC, udsdemo.RACE, udsdemo.EDUC, udsdemo.PRIMLANG, mdem.ENGFL,
lastsubdemo.MARISTAT, lastsubdemo.RESIDENC, onset.DECAGE AS AgeOnset, 
IF(NDemOthRel >=0, NDem1stRel + NDemOthRel, NDem1stRel) AS NDemRel,
mms2.NMMS, blr2.NBlRoth,
IF(lc.LatestDesc IS NULL, 'Not Enrolled', IF(lc.LatestDesc = 'Referred', 'Pending','Enrolled')) AS ADCLC_Enroll, 
IF(lc.LatestDesc IS NULL, 'Not Enrolled', IF(lc.LatestDesc = 'Referred', 'Pending', IF(lc.LatestDesc = 'Enrolled', 'Active', lc.LatestDesc))) AS ADCLC_Status,
c1.LatestDesc C1, c2.LatestDesc C2, ivd.LatestDesc IVD,
hb.LatestDesc Hillblom, adni.LatestDesc ADNI,
aut.LatestDesc AutCons, 
DtLastEval, 
IF(lc.LatestDesc = 'Enrolled', DATE_ADD(DtLastEval, INTERVAL 1 YEAR),'') AS DtNextEval,
-- recode PTRELATION into CHAR eval types
IF(ms.PTRELATION=1, 'Office',
IF(ms.PTRELATION = 2, 'Home',     IF(ms.PTRELATION = 3, 'Phone',
IF(ms.PTRELATION = 4, 'Lost',     IF(ms.PTRELATION = 5, 'Refused',
IF(ms.PTRELATION = 6, 'Deceased', IF(ms.PTRELATION = 7, 'Other',
IF(ms.PTRELATION = 8, 'No Longer Followed','Office')))))))) LastEvalType,

--	Add MRI and PIB    
mri.Last_MRI_Dt, mri.n_mri, 
mri3.Last_MRI_3_Dt, mri3.n_mri_3, 
pib.Last_PIB_Dt, pib.n_pib,
sen.Last_SENAS_Dt, sen.n_senas,
ivdn.Last_IVD_Npsy_Dt, ivdn.n_ivd_npsy,

-- Add Serum/plasma/DNA	
IF(spdna2.n_serum > 0, spdna2.n_serum, NULL) AS n_serum, 
IF(spdna2.n_plasma > 0, spdna2.n_plasma, NULL) AS n_plasma,
IF(spdna2.n_dna > 0, spdna2.n_dna, NULL) AS n_dna,

-- ms.CatalogVType AS LastEvalType,
initialv.VDate AS DtIA, IF(inpsy.MMSE > 30, NULL, inpsy.MMSE) AS MMSEIA, 
iblroth.BLROTH AS BLRTHIA, icdr.CDRGLOB AS CDRIA, imudsdiag.PRIMSYN2 AS SYNDRIA, 
imudsdiag.PRIMET2 AS ETIOLIA, imudsdiag.MIX21 AS MIX1IA, 
imudsdiag.MIX22 AS MIX2IA, 
initialv.MCITypeVis AS MCITYPEIA, lastoffvis.VDate AS DtMROA, 
IF(mrnpsy.MMSE > 30, NULL, mrnpsy.MMSE) AS MMSEMROA, mrroth.BLROTH AS BLRTHMROA,    mrcdr.CDRGLOB AS CDRMROA, mrdiag.PRIMSYN2 AS SYNDRMROA, 
mrdiag.PRIMET2 AS ETIOLMROA, mrdiag.MIX21 AS MIX1MROA, mrdiag.MIX22 AS MIX2MROA, 
lastoffvis.MCITypeVis AS MCITYPEMROA, lastoffvis.ClinAssessN AS RANMROA,
lastbrv.VDate AS DtMRABR, lastbr.BLROTH AS BLRTHMRA, 
lastbrv.ClinAssessN AS RANMRABR, lastdiagv.VDate AS DtMRADx, 
lastmdiag.PRIMSYN2 AS SYNDRMRA, lastmdiag.PRIMET2 AS ETIOLMRA, 
lastmdiag.MIX21 AS MIX1MRA, lastmdiag.MIX22 AS MIX2MRA, 
lastdiagv.MCITypeVis AS MCITYPEMRA,
lastdiagv.ClinAssessN AS RANMRADx, DATE(ms2.DEATHDT) AS DtDeath, 
ms2.AUTOPSY AS Autopsy,
--    IF(np2.FINALDX1 = 11, 7, IF(np2.FINALDX1 = 18, 8, IF(np2.FINALDX1 = 19, 9, IF(np2.FINALDX1 = 20, 10, IF(np2.FINALDX1 = 24, 11, IF(np2.FINALDX1 = 25, 12,
--    IF(np2.FINALDX1 = 26, 13, IF(np2.FINALDX1 = 30, 16, IF(np2.FINALDX1 = 31, 17, IF(np2.FINALDX1 = 35, 19, np2.FINALDX1)))))))))) AS PATHDX1,
--    IF(np2.FINALDX2 = 11, 7, IF(np2.FINALDX2 = 18, 8, IF(np2.FINALDX2 = 19, 9, IF(np2.FINALDX2 = 20, 10, IF(np2.FINALDX2 = 24, 11, IF(np2.FINALDX2 = 25, 12,
--    IF(np2.FINALDX2 = 26, 13, IF(np2.FINALDX2 = 30, 16, IF(np2.FINALDX2 = 31, 17, IF(np2.FINALDX2 = 35, 19, np2.FINALDX2)))))))))) AS PATHDX2,
-- NPATH Dx with naturally coded FINALDX variables 
np2.FINALDX1 AS PATHDX1, np2.FINALdx2 AS PATHDX2, 
np2.BRAAKSTAGE

FROM enrollmentstatus AS e
left join patient p1 using (PIDN)
-- Add Family History
LEFT JOIN (
  SELECT PIDN, fmhx2.NDem1stRel, fmhxr.NDemOthRel,
	IF(fmhxr.NDemOthRel >=0, fmhx2.NDem1stRel + fmhxr.NDemOthRel, fmhx2.NDem1stRel) AS NDemRel
  FROM (
	SELECT PIDN, 
	  max( IF(MOMDEM = '1', '1', '0') + IF(DADDEM = '1', '1', '0') +
		   IF(SIB1DEM = '1', '1', '0') + IF(SIB2DEM = '1', '1', '0') +
		   IF(SIB3DEM = '1', '1', '0') + IF(SIB4DEM = '1', '1', '0') +
		   IF(SIB5DEM = '1', '1', '0') + IF(SIB6DEM = '1', '1', '0') +
		   IF(SIB7DEM = '1', '1', '0') + IF(SIB8DEM = '1', '1', '0') +
		   IF(SIB9DEM = '1', '1', '0') + IF(SIB10DEM = '1', '1', '0') +
		   IF(SIB11DEM = '1', '1', '0') + IF(SIB12DEM = '1', '1', '0') +
		   IF(SIB13DEM = '1', '1', '0') + IF(SIB14DEM = '1', '1', '0') +
		   IF(SIB15DEM = '1', '1', '0') + IF(SIB16DEM = '1', '1', '0') +
		   IF(SIB17DEM = '1', '1', '0') + IF(SIB18DEM = '1', '1', '0') +
		   IF(SIB19DEM = '1', '1', '0') + IF(SIB20DEM = '1', '1', '0')
	  ) as NDem1stRel
	FROM instrumenttracking
	  JOIN udsfamilyhistory2 using (InstrID)
	GROUP BY PIDN
  ) AS fmhx2
  LEFT JOIN(
	SELECT PIDN, Max(RELSDEM) AS NDemOthRel 
	FROM instrumenttracking
	  join udsfamilyhistoryrelatives2 using (InstrID)
	WHERE RELSDEM not in ('-6','99')
	GROUP BY PIDN
  ) AS fmhxr using (PIDN)
) AS fmhx3 using (PIDN)
-- Add Number MMSE
left join (
  SELECT PIDN, COUNT(MMSE) AS NMMS
  FROM instrumenttracking join udsneuropsych using (InstrID)
  WHERE MMSE IS NOT NULL
  GROUP BY PIDN
) AS mms2 using (PIDN)
-- Add Number Blessed Roth
left join (
  select PIDN, count(*) as NBlRoth 
  from instrumenttracking 
  where InstrType='Blessed Roth' 
  group by PIDN
) AS blr2 using (PIDN)

-- Add Most Recent Eval Data
-- Add death date and autopsy
left join (
  SELECT PIDN, DEATHDT, IF(AUTOPSY=1, 1, IF(AUTOPSY = 2, 0, '')) AS AUTOPSY
  FROM instrumenttracking
	join mudsstatus using (InstrID) 
  where DEATHDT is not null or PTRELATION=6
) AS ms2 using (PIDN)
-- Project enrollments
left join enrollmentstatus s1 on s1.ProjName='SENAS' and s1.PIDN=e.PIDN
left join enrollmentstatus lc on lc.ProjName='ADCLC' and lc.PIDN=e.PIDN
left join enrollmentstatus c1 on c1.ProjName='C1' and c1.PIDN=e.PIDN
left join enrollmentstatus c2 on c2.ProjName='C2' and c2.PIDN=e.PIDN
left join enrollmentstatus ivd on ivd.ProjName='IVD' and ivd.PIDN=e.PIDN
left join enrollmentstatus hb on hb.ProjName='Hillblom' and hb.PIDN=e.PIDN
left join enrollmentstatus adni on adni.ProjName='ADNI' and adni.PIDN=e.PIDN
left join enrollmentstatus aut on aut.ProjName='Autopsy' and aut.PIDN=e.PIDN
-- Age of Onset
left join (
  SELECT PIDN, MAX(DCDate) AS DCDate
  FROM instrumenttracking
	join udssymptomsonset using (InstrID)
  where DECAGE not in ('-6','888','999')
  GROUP BY PIDN
) onsetdate on onsetdate.PIDN=e.PIDN
left join instrumenttracking onsettrack on onsettrack.PIDN=e.PIDN and onsettrack.DCDate = onsetdate.DCDate and onsettrack.InstrType='UDS Symptoms Onset'
left join udssymptomsonset onset on onset.InstrID=onsettrack.InstrID
-- English Fluency: 
left join (
  select PIDN, max(DCDate) DCDate
  from instrumenttracking
	join mudsdemographic using (InstrID)
  where ENGFL >= 0
  group by PIDN
) mdemdate on mdemdate.PIDN=e.PIDN
left join instrumenttracking mdemtrack on mdemtrack.PIDN=e.PIDN and mdemtrack.DCDate = mdemdate.DCDate and mdemtrack.InstrType='MUDS Demographic'
left join mudsdemographic mdem on mdem.InstrID=mdemtrack.InstrID
-- ----------- Initial Visit ---------------
left join visit initialv on initialv.PIDN=e.PIDN and initialv.ClinAssessN=1 -- and initialv.VShortDesc like '%Initial%'
  -- UDS demographics
  left join instrumenttracking idemtrack on idemtrack.VID=initialv.VID and idemtrack.InstrType='UDS Subject Demo'
  left join udssubjectdemo udsdemo on udsdemo.InstrID=idemtrack.InstrID
  -- MUDS Diagnosis
  left join instrumenttracking idiagtrack on idiagtrack.VID=initialv.VID and idiagtrack.InstrType='MUDS Diagnosis'
  left join mudsdiagnosis imudsdiag on imudsdiag.InstrID=idiagtrack.InstrID
  -- Blessed Roth
  left join instrumenttracking ibrtrack on ibrtrack.VID=initialv.VID and ibrtrack.InstrType='Blessed Roth'
  left join blessedroth iblroth on iblroth.InstrID=ibrtrack.InstrID
  -- CDR
  left join instrumenttracking icdrtrack on icdrtrack.VID=initialv.VID and icdrtrack.InstrType='UDS CDR'
  left join udscdr icdr on icdr.InstrID=icdrtrack.InstrID
  -- Neuropsych
  left join instrumenttracking inpsytrack on inpsytrack.VID=initialv.VID and inpsytrack.InstrType='UDS Neuropsych'
  left join udsneuropsych inpsy on inpsy.InstrID=inpsytrack.InstrID
-- ---------------- Most Recent Office Eval ------------------------ --
left join (
select PIDN, max(VDate) VDate
from visit where CatalogVType='Office'
group by PIDN
) lastoffvdate on lastoffvdate.PIDN = e.PIDN
left join visit lastoffvis on lastoffvis.PIDN=lastoffvdate.PIDN and lastoffvis.VDate=lastoffvdate.VDate and lastoffvis.CatalogVType='Office'
  -- MUDS Demog
  left join instrumenttracking mrdiagtrack on mrdiagtrack.VID=lastoffvis.VID and mrdiagtrack.InstrType='MUDS Diagnosis'
  left join mudsdiagnosis mrdiag on mrdiag.InstrID=mrdiagtrack.InstrID
  -- Blessed Roth
  left join instrumenttracking mrrothtrack on mrrothtrack.VID=lastoffvis.VID and mrrothtrack.InstrType='Blessed Roth'
  left join blessedroth mrroth on mrroth.InstrID=mrrothtrack.InstrID
  -- CDR
  left join instrumenttracking mrcdrtrack on mrcdrtrack.VID=lastoffvis.VID and mrcdrtrack.InstrType='UDS CDR'
  left join udscdr mrcdr on mrcdr.InstrID=mrcdrtrack.InstrID
  -- Neuropsych
  left join instrumenttracking mrnpsytrack on mrnpsytrack.VID=lastoffvis.VID and mrnpsytrack.InstrType='UDS Neuropsych'
  left join udsneuropsych mrnpsy on mrnpsy.InstrID=mrnpsytrack.InstrID

-- Last Diagnosis
left join (
  SELECT PIDN, MAX(ClinAssessN) AS ClinAssessN -- alistnum
  from visit
	join instrumenttracking using (PIDN,ProjName,VID)
	join mudsdiagnosis using (InstrID)
  where PRIMSYN2 != -9
  GROUP BY PIDN
) lastdiagvnum on lastdiagvnum.PIDN=e.PIDN
left join visit lastdiagv on lastdiagv.PIDN=lastdiagvnum.PIDN and lastdiagv.ClinAssessN=lastdiagvnum.ClinAssessN
left join instrumenttracking lastmdiagtrack on lastmdiagtrack.VID=lastdiagv.VID and lastmdiagtrack.InstrType='MUDS Diagnosis'
left join mudsdiagnosis lastmdiag on lastmdiag.InstrID=lastmdiagtrack.InstrID
-- Last Blessed Roth
left join(
  select PIDN, max(DCDate) DCDate
  from instrumenttracking
	join blessedroth using (InstrID)
  where BLROTH >=0
  group by PIDN
) lastbrdate on lastbrdate.PIDN=e.PIDN
left join instrumenttracking lastbrtrack on lastbrtrack.PIDN=lastbrdate.PIDN and lastbrtrack.DCDate=lastbrdate.DCDate and lastbrtrack.InstrType='Blessed Roth'
left join visit lastbrv on lastbrv.VID=lastbrtrack.VID
left join blessedroth lastbr on lastbr.InstrID=lastbrtrack.InstrID
-- Add Last Marital and Residence Status
left join(
  select PIDN, max(DCDate) DCDate
  from instrumenttracking
	join udssubjectdemo using (InstrID)
  group by PIDN
) lastsubdemodate on lastsubdemodate.PIDN=e.PIDN
left join instrumenttracking lastsubdemotrack on lastsubdemotrack.PIDN=e.PIDN and lastsubdemotrack.DCDate=lastsubdemodate.DCDate and lastsubdemotrack.InstrType='UDS Subject Demo'
left join udssubjectdemo lastsubdemo on lastsubdemo.InstrID=lastsubdemotrack.InstrID
		--  Eventually restore
		-- WHERE (MARISTAT IS NOT NULL OR RESIDENC IS NOT NULL)
		
-- Add MRI 1.5T
LEFT JOIN
(SELECT mr.PIDN, CAST(Max(mr.SCANDT) AS DATE) AS Last_MRI_Dt, COUNT(mr.PIDN) AS n_mri
FROM
(SELECT i.PIDN, m.InstrID, i.DCDATE AS SCANDT
FROM mriquantitative AS m, instrumenttracking AS i
WHERE m.InstrID = i.InstrID AND (m.BM IS NOT NULL OR m.HCVR IS NOT NULL)) AS mr
GROUP BY mr.PIDN) 
AS mri ON mri.PIDN = e.PIDN

-- Add MRI 3.0T
LEFT JOIN
(SELECT mr3.PIDN, CAST(Max(mr3.SCANDT) AS DATE) AS Last_MRI_3_Dt, COUNT(mr3.PIDN) AS n_mri_3
FROM
(SELECT i.PIDN, m3.InstrID, i.DCDATE AS SCANDT
FROM imagingmri AS m3, instrumenttracking AS i
WHERE m3.InstrID = i.InstrID AND m3.SCANNER = 0) AS mr3
GROUP BY mr3.PIDN) 
AS mri3 ON mri3.PIDN = e.PIDN

-- Add PIB
LEFT JOIN
(SELECT pi.PIDN, CAST(Max(pi.SCANDT) AS DATE) AS Last_PIB_Dt, COUNT(pi.PIDN) AS n_pib
FROM
(SELECT i.PIDN, p.InstrID, p.SCANDT
FROM imagingpet AS p, instrumenttracking AS i
WHERE p.InstrID = i.InstrID AND p.STUDYREP = 2) AS pi
GROUP BY pi.PIDN) 
AS pib ON pib.PIDN = e.PIDN

-- Add SENAS
LEFT JOIN
(SELECT se.PIDN, CAST(Max(se.SENAS_Dt) AS DATE) AS Last_SENAS_Dt, COUNT(se.PIDN) AS n_senas
FROM
(SELECT i.PIDN, s.InstrID, i.DCDATE AS SENAS_Dt
FROM senas AS s, instrumenttracking AS i
WHERE s.InstrID = i.InstrID AND (s.VEM11 IS NOT NULL OR s.ANIM1 IS NOT NULL)) AS se
GROUP BY se.PIDN) 
AS sen ON sen.PIDN = e.PIDN

-- Add IVD NPsy
LEFT JOIN
(SELECT ivn.PIDN, CAST(Max(ivn.IVD_Dt) AS DATE) AS Last_IVD_NPsy_Dt, COUNT(ivn.PIDN) AS n_ivd_npsy
FROM
(SELECT i.PIDN, iv.InstrID, i.DCDATE AS IVD_Dt
FROM ivdneuropsych AS iv, instrumenttracking AS i
WHERE iv.InstrID = i.InstrID AND (iv.LLCORRT1 IS NOT NULL OR iv.VFTOTALF IS NOT NULL)) AS ivn
GROUP BY ivn.PIDN) 
AS ivdn ON ivdn.PIDN = e.PIDN

-- Add Serum/Plasma/DNA

LEFT JOIN
(SELECT spdna.PIDN, COUNT(spdna.Serum) AS n_serum, COUNT(spdna.Plasma) AS n_plasma, COUNT(spdna.DNA) AS n_dna  
FROM
(SELECT i.PIDN, p.SubjectStudyID AS ID, c1.ClinAssessN, c1.VisitNum,  c1.ClinVisDt, i.DCDate AS AssessDt, IF(c.serum > 0, c.serum, NULL) AS Serum, IF(c.plasma > 0, c.plasma, NULL) AS Plasma, IF(c.dna > 0, c.dna, NULL) AS DNA
	FROM serumplasmadna AS c
	JOIN instrumenttracking AS i ON i.InstrID = c.InstrID
	JOIN enrollmentstatus AS p ON p.PIDN = i.PIDN AND p.ProjName = 'ADC' AND p.SubjectStudyID + 0 > 0
	LEFT JOIN
	(SELECT i1.PIDN, i1.DCDate, a1.VDate AS ClinVisDt, a1.alistnum AS ClinAssessN, a1.VisitNum, i1.InstrID FROM instrumenttracking AS i1
	JOIN assesslist AS a1 ON a1.PIDN = i1.PIDN AND i1.InstrType='Serum Plasma DNA'
	AND a1.alistnum IS NOT NULL
	AND ABS(DATEDIFF(i1.DCDate, a1.VDate)) <= 180
	AND ABS(DATEDIFF(i1.DCDate, a1.VDate)) = (SELECT MIN(ABS(DATEDIFF(i3.DCDate,a3.VDate)))  FROM assesslist AS a3, instrumenttracking AS i3
	WHERE i3.InstrID = i1.InstrID AND a3.PIDN = i3.PIDN AND a3.alistnum IS NOT NULL 
	GROUP BY i3.InstrID))
	AS c1 ON c1.InstrID = c.InstrID) AS spdna
	GROUP BY spdna.PIDN) AS spdna2
	ON spdna2.PIDN = e.PIDN

		
-- Add Last Status
left join (
  select msfirstdate.PIDN, IF(mslastdate.VDate is null,msfirstdate.VDate,mslastdate.VDate) AS DtLastEval, laststat.PTRELATION
  from (
	select PIDN,VDate
	 from visit
	  join udsvisit using (VID)
	where VisitNum=1
  )msfirstdate
  left join (
	select PIDN, max(VDate) VDate
	from visit
	  join instrumenttracking using (PIDN,ProjName,VID)
	  join mudsstatus using (InstrID)
	where VStatus='COMPLETE'
	group by PIDN
  ) mslastdate on msfirstdate.PIDN=mslastdate.PIDN
  left join(
	select PIDN, VDate, max(msinner.PTRELATION) PTRELATION, max(VisitNum) VisitNum
	from visit
	  left join udsvisit using (VID)
	  join instrumenttracking using (PIDN,ProjName,VID)
	  join mudsstatus msinner using (InstrID)
	group by PIDN,VDate
  ) laststat on laststat.PIDN=mslastdate.PIDN and laststat.VDate = mslastdate.VDate
) ms on ms.PIDN=e.PIDN
-- CERAD
left join instrumenttracking ceradtrack on ceradtrack.PIDN=e.PIDN and ceradtrack.InstrType='CERAD'
left join cerad np2 on np2.InstrID=ceradtrack.InstrID
where e.ProjName='ADC'
group by e.SubjectStudyID
ORDER BY ID;

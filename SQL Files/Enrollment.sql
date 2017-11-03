SELECT e.SubjectStudyID AS ID, e.ProjName, e.LatestDesc, CAST(e.LatestDate AS Date) AS LatestDate, CAST(e.EnrolledDate AS Date) AS EnrolledDate, CAST(e.DroppedDate AS Date) AS DroppedDate, CAST(e.LostDate AS Date) AS LostDate, CAST(e.RefusedDate AS Date) AS RefusedDate, CAST(e.DeceasedDate AS Date) AS DeceasedDate
FROM enrollmentstatus AS e
WHERE e.ProjName = "ADCLC" OR e.ProjName = "C1" OR e.ProjName = "C2" OR e.ProjName = "Hillblom" OR e.ProjName = "IVD" OR e.ProjName = "IVD Legacy" OR e.ProjName = "ADNI" OR e.ProjName = "Autopsy"

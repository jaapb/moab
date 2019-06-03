open Os_db
open Lwt
open CalendarLib

let set_report_feedback ayear userid qc qg ic ig cc cg =
	full_transaction_block (fun dbh -> 
		PGSQL(dbh)
			"INSERT INTO moab.report_scores \
			(academic_year, student_id, quality_feedback, quality_grade, independence_feedback, independence_grade, \
				community_feedback, community_grade) \
			VALUES \
			($ayear, $userid, $qc, $qg, $ic, $ig, $cc, $cg) \
			ON CONFLICT (academic_year, student_id) DO UPDATE \
				SET quality_feedback = EXCLUDED.quality_feedback, \
				quality_grade = EXCLUDED.quality_grade,
				independence_feedback = EXCLUDED.independence_feedback,
				independence_grade = EXCLUDED.independence_grade,
				community_feedback = EXCLUDED.community_feedback,
				community_grade = EXCLUDED.community_grade")


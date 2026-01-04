package analysis

import "testing"

func TestCrossSystemStats_IncludesUncategorized(t *testing.T) {
	commits := []CommitInfo{
		{
			Categories: map[string]int{
				"backend":        2,
				"uncategorized":  1,
			},
		},
		{
			Categories: map[string]int{
				"frontend":       3,
				"uncategorized":  2,
			},
		},
	}

	an := &Analyzer{}
	stats := an.calculateCrossSystemStats(commits)

	if _, ok := stats.SystemTouchMatrix["uncategorized"]; !ok {
		t.Fatalf("expected 'uncategorized' to be present in SystemTouchMatrix")
	}
	found := false
	for _, st := range stats.MostTouchedSystems {
		if st.System == "uncategorized" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected 'uncategorized' to be present in MostTouchedSystems")
	}
}

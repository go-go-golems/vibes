1	package saga
2	
3	import (
4		"context"
5		"fmt"
6		"log"
7		"sync"
8		"time"
9	
10		"github.com/ThreeDotsLabs/watermill/message"
11		"github.com/google/uuid"
12	)
13	
14	// SagaStep defines a step in a saga process
15	type SagaStep struct {
16		// Name of the step for logging and debugging
17		Name string
18		
19		// Handler is the function to execute for this step
20		Handler func(ctx context.Context, data interface{}) (interface{}, error)
21		
22		// Compensation is the function to execute if the saga needs to be reversed
23		Compensation func(ctx context.Context, data interface{}) error
24	}
25	
26	// SagaDefinition defines the sequence of steps in a saga
27	type SagaDefinition struct {
28		// Name of the saga
29		Name string
30		
31		// Steps to execute in order
32		Steps []SagaStep
33	}
34	
35	// SagaInstance represents a running instance of a saga
36	type SagaInstance struct {
37		// ID of this saga instance
38		ID string
39		
40		// Definition is the saga definition
41		Definition SagaDefinition
42		
43		// Data is the data being passed between steps
44		Data interface{}
45		
46		// CurrentStep is the index of the current step
47		CurrentStep int
48		
49		// CompensatingMode indicates if the saga is executing normally or compensating
50		CompensatingMode bool
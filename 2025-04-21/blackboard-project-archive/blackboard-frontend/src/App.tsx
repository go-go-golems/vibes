import React from 'react';
import { useBlackboardState } from './hooks/useBlackboardState';
import { 
  Database, 
  Brain, 
  Activity,
  Target,
  PlayCircle,
  PauseCircle,
  Lock,
  Unlock,
  FileText,
  Radio,
  Zap,
  BarChart
} from 'lucide-react';
import { Hypothesis } from './types';

const App: React.FC = () => {
  const { state, loading, error, cycle } = useBlackboardState();
  const [simulationRunning, setSimulationRunning] = React.useState(false);
  const [lastUpdate, setLastUpdate] = React.useState(new Date().toISOString().replace('T', ' ').slice(0, 19));

  // Update last update time when cycle changes
  React.useEffect(() => {
    setLastUpdate(new Date().toISOString().replace('T', ' ').slice(0, 19));
  }, [cycle]);

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-100">
        <div className="text-xl font-semibold">Loading Blackboard System...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-100">
        <div className="text-xl font-semibold text-red-600">Error: {error}</div>
      </div>
    );
  }

  if (!state) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-100">
        <div className="text-xl font-semibold">No data available</div>
      </div>
    );
  }

  const toggleSimulation = async () => {
    // In a real implementation, this would start/stop the simulation on the backend
    setSimulationRunning(!simulationRunning);
    
    // For now, we'll just make a simple API call to increment the cycle
    if (!simulationRunning) {
      try {
        await fetch('/api/cycle', { method: 'GET' });
      } catch (err) {
        console.error('Error toggling simulation:', err);
      }
    }
  };

  const HypothesisView = ({ hypothesis }: { hypothesis: Hypothesis }) => (
    <div className="border rounded p-2 mb-2 hover:bg-gray-50 transition-colors">
      <div className="flex justify-between items-start">
        <div className="flex items-center gap-2">
          <span className="font-mono text-sm">{hypothesis.id}</span>
          {hypothesis.locked ? 
            <Lock className="w-4 h-4 text-red-500" /> : 
            <Unlock className="w-4 h-4 text-green-500" />
          }
          {hypothesis.owner && (
            <span className="text-xs px-2 py-0.5 bg-gray-200 rounded-full">
              {hypothesis.owner}
            </span>
          )}
        </div>
        <div className="flex items-center gap-2">
          <span className="text-xs text-gray-500">{hypothesis.time_range}</span>
          <span className={`text-xs px-2 py-0.5 rounded-full ${hypothesis.confidence > 0.8 ? 'bg-blue-600 text-white' : 'bg-gray-200'}`}>
            {hypothesis.confidence.toFixed(2)}
          </span>
        </div>
      </div>
      <div className="font-mono text-sm mt-1">{hypothesis.content}</div>
    </div>
  );

  const KnowledgeSourceCard = ({ ks }: { ks: any }) => (
    <div className="border rounded p-3 mb-2">
      <div className="flex justify-between items-start mb-2">
        <div>
          <div className="font-medium">{ks.name}</div>
          <div className="text-xs text-gray-500">{ks.activation_pattern}</div>
        </div>
        <span className={`text-xs px-2 py-0.5 rounded-full ${
          ks.status === 'active' ? 'bg-blue-600 text-white' : 
          ks.status === 'activating' ? 'bg-blue-200' : 'bg-gray-200'
        }`}>
          {ks.status}
        </span>
      </div>
      <div className="text-sm text-gray-600">{ks.last_action}</div>
      <div className="mt-2 flex justify-between items-center">
        <span className="text-xs text-gray-500">Bid Value:</span>
        <span className="text-xs px-2 py-0.5 bg-gray-200 rounded-full">
          {ks.bid_value.toFixed(2)}
        </span>
      </div>
    </div>
  );

  return (
    <div className="w-full max-w-7xl mx-auto p-4 space-y-4 bg-gray-50">
      {/* Header */}
      <div className="bg-gradient-to-r from-blue-800 to-blue-600 text-white rounded-lg p-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">HEARSAY-II BLACKBOARD SYSTEM</h1>
            <p className="text-blue-100">Speech Understanding Through Collaborative Processing</p>
          </div>
          <div className="text-right">
            <div className="font-mono">CYCLE: {cycle}</div>
            <div className="text-sm text-blue-100">TIME: {lastUpdate}</div>
            <button 
              onClick={toggleSimulation}
              className="mt-2 flex items-center gap-2 bg-white text-blue-800 px-4 py-2 rounded-md hover:bg-blue-50"
            >
              {simulationRunning ? 
                <><PauseCircle className="w-4 h-4" /> Pause</> : 
                <><PlayCircle className="w-4 h-4" /> Start</>
              }
            </button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-4">
        {/* Main Blackboard Panel - 3 columns */}
        <div className="lg:col-span-3 space-y-4">
          <div className="bg-white rounded-lg shadow p-4">
            <h2 className="text-lg font-semibold flex items-center gap-2 mb-4">
              <Database className="w-5 h-5" />
              Hypothesis Blackboard
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {/* Phrase Level */}
              <div className="space-y-2">
                <h3 className="font-semibold text-sm text-gray-700 flex items-center gap-2">
                  <FileText className="w-4 h-4" /> PHRASE LEVEL
                </h3>
                {state.phrase.map(hyp => (
                  <HypothesisView key={hyp.id} hypothesis={hyp} />
                ))}
              </div>

              {/* Word Level */}
              <div className="space-y-2">
                <h3 className="font-semibold text-sm text-gray-700 flex items-center gap-2">
                  <FileText className="w-4 h-4" /> WORD LEVEL
                </h3>
                {state.word.map(hyp => (
                  <HypothesisView key={hyp.id} hypothesis={hyp} />
                ))}
              </div>

              {/* Syllable Level */}
              <div className="space-y-2">
                <h3 className="font-semibold text-sm text-gray-700 flex items-center gap-2">
                  <Radio className="w-4 h-4" /> SYLLABLE LEVEL
                </h3>
                {state.syllable.map(hyp => (
                  <HypothesisView key={hyp.id} hypothesis={hyp} />
                ))}
              </div>

              {/* Segment Level */}
              <div className="space-y-2">
                <h3 className="font-semibold text-sm text-gray-700 flex items-center gap-2">
                  <Zap className="w-4 h-4" /> SEGMENT LEVEL
                </h3>
                {state.segment.map(hyp => (
                  <HypothesisView key={hyp.id} hypothesis={hyp} />
                ))}
              </div>

              {/* Parameter Level */}
              <div className="space-y-2 lg:col-span-2">
                <h3 className="font-semibold text-sm text-gray-700 flex items-center gap-2">
                  <BarChart className="w-4 h-4" /> PARAMETER LEVEL
                </h3>
                {state.parameter.map(hyp => (
                  <HypothesisView key={hyp.id} hypothesis={hyp} />
                ))}
              </div>
            </div>
          </div>

          {/* Activity Log */}
          <div className="bg-white rounded-lg shadow p-4">
            <h2 className="text-lg font-semibold flex items-center gap-2 mb-4">
              <Activity className="w-5 h-5" />
              System Activity Log
            </h2>
            <div className="space-y-2 max-h-48 overflow-y-auto">
              {state.activity_log.map((entry, i) => (
                <div key={i} className="flex items-center gap-3 text-sm">
                  <span className="font-mono text-xs text-gray-500 w-24">{entry.time}</span>
                  <span className={`text-xs px-2 py-0.5 rounded-full ${entry.level === 'CONTROL' ? 'bg-red-100' : 'bg-gray-200'} w-20 text-center`}>
                    {entry.knowledge_source_id}
                  </span>
                  <span>{entry.action}</span>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Right Panel - 1 column */}
        <div className="space-y-4">
          {/* Knowledge Sources */}
          <div className="bg-white rounded-lg shadow p-4">
            <h2 className="text-lg font-semibold flex items-center gap-2 mb-4">
              <Brain className="w-5 h-5" />
              Knowledge Sources
            </h2>
            {state.knowledge_sources.map(ks => (
              <KnowledgeSourceCard key={ks.id} ks={ks} />
            ))}
          </div>

          {/* Focus-of-Control */}
          <div className="bg-white rounded-lg shadow p-4">
            <h2 className="text-lg font-semibold flex items-center gap-2 mb-4">
              <Target className="w-5 h-5" />
              Focus-of-Control
            </h2>
            <div className="space-y-4">
              <div>
                <div className="text-sm text-gray-500 mb-1">Current Focus:</div>
                <span className="text-lg px-3 py-1 bg-blue-600 text-white rounded-md">
                  {state.focus.current_focus}
                </span>
              </div>
              <div>
                <div className="text-sm text-gray-500 mb-2">Level Priorities:</div>
                {Object.entries(state.focus.focus_priorities).map(([level, priority]) => (
                  <div key={level} className="flex justify-between items-center mb-1">
                    <span className="text-sm">{level}</span>
                    <div className="w-32 bg-gray-200 rounded-full h-2">
                      <div 
                        className="bg-blue-600 h-2 rounded-full" 
                        style={{ width: `${priority * 100}%` }}
                      />
                    </div>
                    <span className="text-sm">{(priority * 100).toFixed(0)}%</span>
                  </div>
                ))}
              </div>
              <div>
                <div className="text-sm text-gray-500 mb-2">Bid Queue:</div>
                {state.focus.bid_queue.map((bid, i) => (
                  <div key={i} className="text-sm border-b py-1">
                    <div className="flex justify-between">
                      <span>{bid.knowledge_source_id}</span>
                      <span className="font-mono">{bid.bid_value.toFixed(2)}</span>
                    </div>
                    <div className="text-xs text-gray-600">{bid.action}</div>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default App;

import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, PieChart, Pie, Cell, LineChart, Line } from 'recharts'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'

const COLORS = ['#10b981', '#3b82f6', '#8b5cf6', '#f59e0b']

export function InteractiveCharts() {
  const minificationData = [
    {
      method: 'Terser',
      originalSize: 544098,
      minifiedSize: 71124,
      reduction: 86.9,
      performance: 8.55,
      color: '#10b981'
    },
    {
      method: 'tdewolff',
      originalSize: 544098,
      minifiedSize: 71895,
      reduction: 86.8,
      performance: 12.06,
      color: '#3b82f6'
    },
    {
      method: 'esbuild',
      originalSize: 544098,
      minifiedSize: 72859,
      reduction: 86.6,
      performance: 13.91,
      color: '#8b5cf6'
    }
  ]

  const performanceData = [
    { method: 'esbuild', performance: 13.91, time: 719 },
    { method: 'tdewolff', performance: 12.06, time: 830 },
    { method: 'terser', performance: 8.55, time: 1169 }
  ]

  const sizeComparisonData = [
    { name: 'Original', size: 544098, percentage: 100 },
    { name: 'Terser', size: 71124, percentage: 13.1 },
    { name: 'tdewolff', size: 71895, percentage: 13.2 },
    { name: 'esbuild', size: 72859, percentage: 13.4 }
  ]

  const lodashFunctionsData = [
    { category: 'Array', functions: 8, tested: 8 },
    { category: 'Collection', functions: 10, tested: 10 },
    { category: 'Object', functions: 9, tested: 9 },
    { category: 'String', functions: 12, tested: 12 },
    { category: 'Math', functions: 11, tested: 11 },
    { category: 'Utility', functions: 11, tested: 11 }
  ]

  return (
    <div className="space-y-8">
      {/* Size Comparison Chart */}
      <Card>
        <CardHeader>
          <CardTitle>File Size Comparison</CardTitle>
          <CardDescription>
            Visual comparison of original vs minified lodash sizes
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={sizeComparisonData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" />
              <YAxis tickFormatter={(value) => `${(value / 1000).toFixed(0)}KB`} />
              <Tooltip 
                formatter={(value) => [`${(value / 1000).toFixed(1)}KB`, 'Size']}
                labelFormatter={(label) => `Method: ${label}`}
              />
              <Bar dataKey="size" fill="#3b82f6" />
            </BarChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>

      {/* Performance Comparison */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
        <Card>
          <CardHeader>
            <CardTitle>Performance Comparison</CardTitle>
            <CardDescription>
              Operations per millisecond for each minification method
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={250}>
              <BarChart data={performanceData} layout="horizontal">
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis type="number" />
                <YAxis dataKey="method" type="category" width={80} />
                <Tooltip 
                  formatter={(value) => [`${value} ops/ms`, 'Performance']}
                />
                <Bar dataKey="performance" fill="#10b981" />
              </BarChart>
            </ResponsiveContainer>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Size Reduction Distribution</CardTitle>
            <CardDescription>
              Percentage breakdown of minified sizes
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={250}>
              <PieChart>
                <Pie
                  data={minificationData}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ method, reduction }) => `${method}: ${reduction}%`}
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="reduction"
                >
                  {minificationData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                  ))}
                </Pie>
                <Tooltip formatter={(value) => [`${value}%`, 'Reduction']} />
              </PieChart>
            </ResponsiveContainer>
          </CardContent>
        </Card>
      </div>

      {/* Function Coverage */}
      <Card>
        <CardHeader>
          <CardTitle>Lodash Function Coverage</CardTitle>
          <CardDescription>
            Comprehensive testing across all lodash function categories
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={lodashFunctionsData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="category" />
              <YAxis />
              <Tooltip />
              <Bar dataKey="functions" fill="#e5e7eb" name="Total Functions" />
              <Bar dataKey="tested" fill="#10b981" name="Tested Functions" />
            </BarChart>
          </ResponsiveContainer>
          <div className="mt-4 text-center">
            <div className="inline-flex items-center space-x-4 text-sm">
              <div className="flex items-center">
                <div className="w-3 h-3 bg-gray-300 rounded mr-2"></div>
                <span>Total Functions</span>
              </div>
              <div className="flex items-center">
                <div className="w-3 h-3 bg-green-500 rounded mr-2"></div>
                <span>Tested & Working</span>
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Performance Timeline */}
      <Card>
        <CardHeader>
          <CardTitle>Performance vs Size Trade-off</CardTitle>
          <CardDescription>
            Relationship between file size and runtime performance
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={minificationData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="minifiedSize" 
                tickFormatter={(value) => `${(value / 1000).toFixed(0)}KB`}
              />
              <YAxis 
                label={{ value: 'Performance (ops/ms)', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip 
                formatter={(value, name) => [
                  name === 'performance' ? `${value} ops/ms` : `${(value / 1000).toFixed(1)}KB`,
                  name === 'performance' ? 'Performance' : 'Size'
                ]}
                labelFormatter={(value) => `Size: ${(value / 1000).toFixed(1)}KB`}
              />
              <Line 
                type="monotone" 
                dataKey="performance" 
                stroke="#3b82f6" 
                strokeWidth={3}
                dot={{ fill: '#3b82f6', strokeWidth: 2, r: 6 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>
    </div>
  )
}


const path = require('path');

module.exports = {
  mode: 'development',
  entry: './src/enhanced-tui.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'simple-tui-bundle.js',
    library: 'SimpleTUILib',
    libraryTarget: 'var',
    globalObject: 'this'
  },
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['@babel/preset-env', {
                targets: {
                  browsers: ['ie >= 9']
                },
                modules: false
              }],
              '@babel/preset-react'
            ]
          }
        }
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.jsx']
  },
  target: ['web', 'es5']
};


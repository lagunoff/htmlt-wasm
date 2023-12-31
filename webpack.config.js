const path = require('path');

module.exports = {
  entry: './src-js/index.js',
  output: {
    path: path.resolve(__dirname, 'dist-newstyle'),
    filename: 'index.bundle.js',
  },
  resolve: {
    extensions: ['.ts', '.js'],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  devtool: false,
};

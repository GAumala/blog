const path = require('path');
const { spawn } = require('child_process');

const showLogs = process.env.LOGS;

const runServer = ({ name, exe, args, cwd, readyMessage, timeToWait }) => 
  new Promise((resolve, reject) => {
    const cp = spawn(exe, args, { cwd });

    cp.stdout.on('data', data => {
      const logMsg = data.toString();
      if (showLogs === 'stdout' || showLogs == 'all')
        console.log(`[${exe}] [stdout]`, logMsg);
      if (logMsg.indexOf(readyMessage) !== -1) {
        resolve({ kill: () => cp.kill() })
      }
    })
    cp.stderr.on('data', data => {
      const logMsg = data.toString();
      if (showLogs === 'stderr' || showLogs == 'all')
        console.log(`[${exe}] [stderr]`, logMsg);
      if (logMsg.indexOf(readyMessage) !== -1) {
        resolve({ kill: () => cp.kill() })
      }
    });

    cp.on('error', (err) => {
      reject(err);
    });
  });

const runDevServer = () => {
  const name = 'dev-server';
  const exe = 'yarn';
  const args = ['start'];
  const cwd = path.join(__dirname, '../');
  const readyMessage = 'Compiled successfully.'
  return runServer({ name, exe, args, cwd, readyMessage }) 
}

const runAPIServer = () => {
  const name = 'api-server';
  const exe = 'yarn';
  const args = ['start'];
  const cwd = path.join(__dirname, '../../api');
  const readyMessage = 'ready'
  return runServer({ name, exe, args, cwd, readyMessage }) 
}

module.exports = { runDevServer, runAPIServer }

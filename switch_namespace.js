const fs = require('fs');
const path = require('path');

if (process.argv.length <= 4) {
  console.log(`Usage: ${__filename} <path/to/directory> <old string> <new string>`);
  process.exit(-1);
}
const currDir = process.argv[2];
const stats = fs.statSync(currDir);
if (!stats.isDirectory()) {
  console.log(`${currDir} is not a directory. \n exiting...`);
  return;
}

function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
}

const oldStr = process.argv[3].toLowerCase();
const oldUpperStr = oldStr.toUpperCase();
const oldRegex = new RegExp(escapeRegExp(oldStr), 'g');
const oldUpperRegex = new RegExp(escapeRegExp(oldUpperStr), 'g');
const oldHashStr = oldStr.replace(/\//g, '#');
const oldHashUpperStr = oldHashStr.toUpperCase();

const newStr = process.argv[4].toLowerCase();
const newUpperStr = newStr.toUpperCase();
// const newRegex = new RegExp(escapeRegExp(newStr), 'g');
// const newUpperRegex = new RegExp(escapeRegExp(newUpperStr), 'g');
const newHashStr = newStr.replace(/\//g, '#');
const newHashUpperStr = newHashStr.toUpperCase();

console.log(`replacing '${oldStr}' with '${newStr}' under folder '${currDir}'`);

function processFileContents(filePath) {
  const buffer = fs.readFileSync(filePath, { encoding: 'UTF8' });
  let newBuffer = buffer.replace(oldRegex, newStr);
  newBuffer = newBuffer.replace(oldUpperRegex, newUpperStr);
  console.log(`\tUpdating File Contents...`);
  fs.writeFileSync(filePath, newBuffer, { encoding: 'UTF8' });
}

function processFile(parentPath, fileName) {
  if (fileName.match(/.pdf|.DS_Store/gi))
    return;

  const filePath = path.join(parentPath, fileName);
  console.log(`\tProcessing File: ${fileName}:`);

  // replace file contents...
  processFileContents(filePath);

  // rename directory after processing all files in it
  let newFileName = fileName.replace(oldHashStr, newHashStr);
  newFileName = newFileName.replace(oldHashUpperStr, newHashUpperStr);
  if (fileName !== newFileName) {
    console.log(`\tRenaming File to: ${newFileName}`);
    newFileName = path.join(parentPath, newFileName);
    fs.renameSync(filePath, newFileName);
  }
}

function processDir(parentPath, dirName) {
  if (dirName === '.git')
    return;
  const dirPath = path.join(parentPath, dirName);
  console.log(`\nProcessing Directory: ${dirPath}:`);
  const files = fs.readdirSync(dirPath);
  files.forEach(item => {
    const filePath = path.join(dirPath, item);
    const stats = fs.statSync(filePath);
    if (stats.isDirectory()) {
      processDir(dirPath, item);
    } else {
      processFile(dirPath, item);
    }
  })

  // rename directory after processing all files in it
  let newDirName = dirName.replace(oldHashStr, newHashStr);
  newDirName = newDirName.replace(oldHashUpperStr, newHashUpperStr);
  if (dirName !== newDirName) {
    console.log(`Renaming Directory to: ${newDirName}`);
    newDirName = path.join(parentPath, newDirName);
    fs.renameSync(dirPath, newDirName);
  }
}

const currDirPath = path.parse(currDir);
processDir(currDirPath.dir, currDirPath.base);
#!/usr/bin/env node

const { readdir, readFile, writeFile } = require("fs").promises;
const path = require("path");
const TOKEN_REGISTRY_REPO_DIR = process.env['TOKEN_REGISTRY_REPO']

const main = async () => {
  const dirName = path.join(TOKEN_REGISTRY_REPO_DIR, "mappings");
  console.log(`Reading token registry from ${dirName}`);
  const fileList = await readdir(dirName);
  const jsonFileList = fileList
    .filter(f => f.toLowerCase().endsWith('.json'))
    .sort(f => f);

  const tokenDetails = [];
  await Promise.all(jsonFileList.map(async fileName => {
    const fullFileName = path.join(dirName, fileName);
    const data = await readFile(fullFileName);

    const fileContent = JSON.parse(data);
    if (fileContent.ticker) {
      tokenDetails.push({
        assetId: fileContent.subject,
        ticker: fileContent.ticker.value,
        decimals: fileContent.decimals?.value
      });
    }
  }));

  await writeFile('token-details.json', JSON.stringify(tokenDetails, null, 2));
  console.log('Generated token-details.json');
};

main();

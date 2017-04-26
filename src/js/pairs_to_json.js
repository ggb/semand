const fs = require('fs')

fs.readFile('band.pairs', 'utf8', (err,data) => {
  if (err) return console.log(err)
  
  const links = data.split("\n").map(row => {
    if(row == "") return
    const splittedRow = row.replace(/\r/,"").split(" ")
    return {
      source: splittedRow[0],
      target: splittedRow[1],
      value: 3
    }
  })

  const nodes = links.reduce((acc, ele) => {
    if(ele && acc.indexOf(ele.source) === -1) {
      acc.push(ele.source)
    }
    if(ele && acc.indexOf(ele.target) === -1) {
      acc.push(ele.target)
    }
    return acc
  }, []).map(ele => {
    return {id:ele, group: 1}
  })

  const result = JSON.stringify({links,nodes})
  fs.writeFile('bands.json', result, function (err) {
    if (err) return console.log(err)
    console.log(`Wrote ${nodes.length} nodes and ${links.length} links to bands.json.`)
  });
});
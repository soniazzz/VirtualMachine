//can comment out to see what happens without garbage collection but this edition of VM has some minor bugs, will produce error message if you run large recursion 
// const run_vm=require('./VM.cjs')

//This is the ultimate VM with garbage collection
const run_vm = require('./VMwithGC.cjs')
const express = require('express')
const cors = require('cors')

const app = express()

app.use(cors())
app.use(express.json())

app.post('/run-vm', (req, res) => {
  try {
    const { message } = req.body
    run_vm(message)
      .then(({ result }) => {
        console.log("Here is the result to be printed out")
        console.log(result)
        res.json({ success: true, reply: result })
      })
      .catch((error) => {
        console.error('Error running VM:', error)
        res.status(500).json({ success: false, reply: 'Internal server error' })
      })
  } catch (error) {
    console.error('Error:', error)
    res.status(500).json({ success: false, reply: 'Internal server error' })
  }
})

const PORT = process.env.PORT || 3001
app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`)
})

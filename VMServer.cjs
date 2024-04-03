const run_vm=require('./VM.cjs')
const express = require('express')
const cors = require('cors')

const app = express()

app.use(cors())
app.use(express.json())

app.post('/run-vm', (req, res) => {
  try {
    const { message } = req.body
    const { result } = run_vm(message)
    res.json({ success: true, reply: result })
  } catch (error) {
    console.error('Error running VM:', error)
    res.status(500).json({ success: false, reply: 'Internal server error' })
  }
})

const PORT = process.env.PORT || 3001
app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`)
})
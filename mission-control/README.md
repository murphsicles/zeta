# 🏭 OpenClaw Mission Control

Visual dashboard for monitoring and managing OpenClaw Dark Factory operations.

## 🚀 Features

### **Core Dashboard**
- Real-time system status monitoring
- Agent management and coordination
- Task queue visualization
- Activity timeline
- Performance metrics

### **14 Integrated Modules**
1. **📊 Dashboard** - System overview
2. **✅ Tasks** - Work queue management
3. **🤖 Agents** - AI agent coordination
4. **📝 Content** - Generated content repository
5. **🔄 Approvals** - Decision queue
6. **🏛️ Council** - AI coordination & voting
7. **📅 Calendar** - Schedule & milestones
8. **🚀 Projects** - Project management
9. **🧠 Memory** - Agent memory & context
10. **🏢 Office** - Workspace management
11. **👥 Team** - Human-AI team structure
12. **🏭 Factory** - Dark Factory pipeline
13. **⚙️ Pipeline** - CI/CD & build processes
14. **🔬 AI Lab** - Experiments & research
15. **💬 Feedback** - Performance tuning

## 🛠️ Installation

### **Option 1: Quick Start (Windows)**
```bash
# 1. Navigate to mission-control folder
cd C:\Users\mummy\.openclaw\workspace\mission-control

# 2. Install dependencies
npm install

# 3. Start the server
npm start
```

### **Option 2: Development Mode**
```bash
npm run dev  # Auto-restarts on changes
```

### **Option 3: One-click Start (Windows)**
Run `start-mission-control.bat` from the folder.

## 🌐 Access

Once running, open your browser to:
- **Dashboard:** http://localhost:3000
- **API Status:** http://localhost:3000/api/status
- **WebSocket:** ws://localhost:3000 (real-time updates)

## 🔌 Integration with OpenClaw

### **Current Features:**
- Real-time WebSocket connections
- Simulated agent status updates
- Task management interface
- Activity logging

### **Planned Integrations:**
1. **OpenClaw API Connection** - Live agent data
2. **GitHub Integration** - Repository monitoring
3. **CI/CD Pipeline** - Build status
4. **Memory System** - Agent context sharing
5. **Approval System** - Human-in-the-loop decisions

## 🏗️ Architecture

```
Frontend (Browser)
    ↓
WebSocket ←→ Real-time updates
    ↓
Express Server (Node.js)
    ↓
OpenClaw API → Live agent data
    ↓
Data Storage (JSON/DB)
```

### **Tech Stack:**
- **Frontend:** HTML5, CSS3, Vanilla JavaScript
- **Backend:** Node.js, Express
- **Real-time:** WebSocket
- **Data:** JSON APIs, simulated data
- **Visualization:** Custom CSS, Charts.js (planned)

## 🎯 Use Cases

### **For Zeta Foundation:**
- Monitor bootstrap compiler progress
- Coordinate specialized AI agents
- Track project milestones
- Manage AI council decisions

### **For Dark Factory:**
- Visualize production pipeline
- Monitor 24/7 worker status
- Track CI/CD builds
- Manage memory and context

### **For Team Management:**
- Assign tasks to AI agents
- Review generated content
- Approve critical decisions
- Tune agent behavior

## 🔧 Configuration

### **Environment Variables:**
Create `.env` file:
```env
PORT=3000
OPENCLAW_API_URL=http://localhost:8080
GITHUB_TOKEN=your_token_here
```

### **Agent Configuration:**
Edit `data/agents.json` to add/remove agents:
```json
{
  "name": "Dark Factory",
  "role": "Main Assistant",
  "model": "deepseek-chat",
  "capabilities": ["coding", "planning", "coordination"],
  "memory_size": "10K tokens"
}
```

## 📈 Roadmap

### **Phase 1 (Now)**
- ✅ Basic dashboard with simulated data
- ✅ WebSocket real-time updates
- ✅ Agent management interface
- ✅ Task queue visualization

### **Phase 2 (Next)**
- OpenClaw API integration
- GitHub repository monitoring
- Real agent status from OpenClaw
- Memory system integration

### **Phase 3 (Future)**
- AI Council voting system
- Advanced visualization (D3.js)
- Performance analytics
- Mobile responsive design
- Plugin system for extensions

## 🐛 Troubleshooting

### **Common Issues:**

1. **Port 3000 already in use**
   ```bash
   # Change port in server.js or use:
   PORT=4000 npm start
   ```

2. **WebSocket connection failed**
   - Check if server is running
   - Ensure firewall allows port 3000
   - Try http://localhost:3000 in browser first

3. **No data showing**
   - Check browser console for errors
   - Verify server is responding to /api/status
   - Refresh the page

### **Logs:**
- Server logs in terminal
- Browser console (F12) for frontend issues
- WebSocket messages in browser network tab

## 🤝 Contributing

This is part of the Zeta Foundation Dark Factory project. To contribute:

1. Fork the repository
2. Create feature branch
3. Make changes
4. Submit pull request

## 📄 License

MIT License - See LICENSE file for details.

## 🏭 Built by Dark Factory AI

Part of the OpenClaw ecosystem for Zeta Foundation bootstrap development.

---

**Start exploring:** `npm start` then open http://localhost:3000
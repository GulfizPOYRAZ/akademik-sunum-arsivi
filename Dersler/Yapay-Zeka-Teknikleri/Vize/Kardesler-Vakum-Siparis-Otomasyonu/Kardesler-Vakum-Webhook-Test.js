// Node.js ile n8n webhook POST testi
import fetch from "node-fetch";    // ESM formatı

async function sendRequest() {
  const url = "http://localhost:5678/webhook-test/Sipariş";

  const bodyData = {
    musteri: "Elif Poyraz",
    email: "elif@example.com",
    urun: "Mama",
    miktar: 2
  };

  const response = await fetch(url, {
    method: "POST",
    headers: { 
      "Content-Type": "application/json" 
    },
    body: JSON.stringify(bodyData)
  });

  const result = await response.text();
  console.log("Webhook yanıtı:", result);
}

sendRequest();

import readline from 'readline'

export default async function promptUser( prompt = "Enter message: " ) {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    
    return new Promise((resolve, reject) => {
        rl.question(prompt, (response) => {
            rl.close();
            resolve(response);
        });
        
        rl.on('SIGINT', () => {
            rl.close();
            reject(new Error('User cancelled the prompt.'));
        });
    });
}

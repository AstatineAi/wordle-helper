import { WASI } from "@bjorn3/browser_wasi_shim";

// UI Logic
const grid = document.getElementById('grid');
const keyboard = document.getElementById('keyboard');
const rows = 6;
const cols = 5;

// Initialize Grid
for (let r = 0; r < rows; r++) {
    const rowDiv = document.createElement('div');
    rowDiv.className = 'row';
    for (let c = 0; c < cols; c++) {
        const cell = document.createElement('div');
        cell.className = 'cell absent'; // Default to absent (gray)
        cell.dataset.row = r;
        cell.dataset.col = c;
        cell.dataset.color = 'absent'; // absent, present, correct
        cell.addEventListener('click', () => cycleColor(cell));
        rowDiv.appendChild(cell);
    }
    grid.appendChild(rowDiv);
}

// Initialize Keyboard
const keys = [
  ['q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'],
  ['a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l'],
  ['z', 'x', 'c', 'v', 'b', 'n', 'm', 'Backspace']
];

keys.forEach(rowKeys => {
  const rowDiv = document.createElement('div');
  rowDiv.className = 'keyboard-row';
  rowKeys.forEach(key => {
    const button = document.createElement('button');
    button.textContent = key === 'Backspace' ? '⌫' : key;
    button.className = key === 'Backspace' ? 'key wide' : 'key';
    button.addEventListener('click', () => {
      if (key === 'Backspace') {
        removeLetter();
      } else {
        addLetter(key);
      }
    });
    rowDiv.appendChild(button);
  });
  keyboard.appendChild(rowDiv);
});

function cycleColor(cell) {
    const colors = ['absent', 'present', 'correct'];
    const currentState = cell.dataset.color;
    const nextIndex = (colors.indexOf(currentState) + 1) % colors.length;
    const nextColor = colors[nextIndex];
    
    cell.className = `cell ${nextColor}`;
    cell.dataset.color = nextColor;
}

// Keyboard Input
document.addEventListener('keydown', (e) => {
    if (e.key.match(/^[a-z]$/i)) {
        addLetter(e.key.toLowerCase());
    } else if (e.key === 'Backspace') {
        removeLetter();
    }
});

function addLetter(letter) {
    // Find the first empty cell
    for(let r=0; r<rows; r++) {
        for(let c=0; c<cols; c++) {
            const cell = getCell(r, c);
            if (!cell.innerText) {
                cell.innerText = letter;
                return;
            }
        }
    }
}

function removeLetter() {
    // Find last filled cell
    for(let r=rows-1; r>=0; r--) {
        for(let c=cols-1; c>=0; c--) {
            const cell = getCell(r, c);
            if (cell.innerText) {
                cell.innerText = '';
                return;
            }
        }
    }
}

function getCell(r, c) {
    return grid.children[r].children[c];
}

// Collect Guesses
function getGuesses() {
    const guesses = [];
    for(let r=0; r<rows; r++) {
        let word = "";
        let colors = "";
        let hasContent = false;
        
        for(let c=0; c<cols; c++) {
            const cell = getCell(r, c);
            const letter = cell.innerText;
            if (letter) {
                hasContent = true;
                word += letter;
                const color = cell.dataset.color;
                if (color === 'correct') colors += 'g';
                else if (color === 'present') colors += 'y';
                else colors += 'x'; // Gray
            }
        }
        
        if (hasContent && word.length === 5) {
            guesses.push(`${word}:${colors}`);
        }
    }
    return guesses;
}

// WASM Loading
async function initWasm() {
    const wasi = new WASI([], [], []);
    
    const __exports = {};
    let ghc_wasm_jsffi = {};

    // Try to load ghc_wasm_jsffi.js
    try {
        const jsffi = await import("./ghc_wasm_jsffi.js");
        ghc_wasm_jsffi = jsffi.default(__exports);
    } catch (e) {
        console.warn("ghc_wasm_jsffi.js not found or failed to load.", e);
    }

    let wasmImports = {
        wasi_snapshot_preview1: wasi.wasiImport,
        ghc_wasm_jsffi: ghc_wasm_jsffi
    };

    try {
        const { instance } = await WebAssembly.instantiateStreaming(
            fetch(import.meta.env.BASE_URL + 'wordle-helper.wasm'), 
            wasmImports
        );
        
        Object.assign(__exports, instance.exports);
        wasi.initialize(instance);
        console.log("WASM initialized");
        
        // Call the exported Haskell entry point
        if (instance.exports.hs_start) {
            console.log("Calling hs_start...");
            instance.exports.hs_start();
            console.log("hs_start returned.");
            enableControls();
        } else {
            console.error("hs_start export not found!");
        }
    } catch (e) {
        console.error("Failed to load WASM:", e);
    }
}

function enableControls() {
    document.getElementById('suggest-new').disabled = false;
    document.getElementById('suggest-best').disabled = false;
    document.getElementById('reset').disabled = false;
}

function disableControls() {
    document.getElementById('suggest-new').disabled = true;
    document.getElementById('suggest-best').disabled = true;
    document.getElementById('reset').disabled = true;
}

disableControls();
initWasm();

// Helper to wrap callback-based Haskell function into a Promise
function callHaskell(funcName, arg) {
    return new Promise((resolve) => {
        if (window[funcName]) {
            window[funcName](arg, resolve);
        } else {
            console.error(funcName + " not found");
            resolve("[]");
        }
    });
}

// Buttons
document.getElementById('suggest-new').addEventListener('click', async () => {
    const guesses = getGuesses();
    console.log("Sending guesses:", guesses);
    const suggestions = await callHaskell('suggestNewWords', JSON.stringify(guesses));
    displaySuggestions(JSON.parse(suggestions));
});

document.getElementById('suggest-best').addEventListener('click', async () => {
    const guesses = getGuesses();
    console.log("Sending guesses:", guesses);
    const suggestions = await callHaskell('suggestBestWords', JSON.stringify(guesses));
    displaySuggestions(JSON.parse(suggestions));
});

document.getElementById('reset').addEventListener('click', () => {
    document.querySelectorAll('.cell').forEach(c => {
        c.innerText = '';
        c.className = 'cell absent';
        c.dataset.color = 'absent';
    });
    document.getElementById('suggestion-list').innerHTML = '';
});

function displaySuggestions(list) {
    const ul = document.getElementById('suggestion-list');
    ul.innerHTML = '';
    list.forEach(word => {
        const li = document.createElement('li');
        li.className = 'suggestion-item';
        li.innerText = word;
        ul.appendChild(li);
    });
}

import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';


function Square(props) {  // functional component: just a function that takes props and returns what to be rendered
    return (
        <button
            className="square"
            onClick={props.onClick}
            style={{backgroundColor: props.highlight ? "#F6BB42" : "#FFFFFF"}}
        >
            {props.value}
        </button>
    );
}

class Board extends React.Component {
    renderSquare(i) {
        return (
            <Square
                value={this.props.squares[i]}
                highlight={this.props.highlight.indexOf(i) !== -1}
                onClick={() => this.props.onClick(i)}
            />
        );
    }

    render() {
        return (
            <div>
                <div className="board-row">
                    {this.renderSquare(0)}
                    {this.renderSquare(1)}
                    {this.renderSquare(2)}
                </div>
                <div className="board-row">
                    {this.renderSquare(3)}
                    {this.renderSquare(4)}
                    {this.renderSquare(5)}
                </div>
                <div className="board-row">
                    {this.renderSquare(6)}
                    {this.renderSquare(7)}
                    {this.renderSquare(8)}
                </div>
            </div>
        );
    }
}

class Game extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            history: [{
                squares: Array(9).fill(null),
                pos: null  // position of the move
            }],
            xIsNext: true,
            stepNumber: 0,
            sortAscending: true
        }

    }
    handleClick(i) {
        const history = this.state.sortAscending
            ? this.state.history.slice(0, this.state.stepNumber + 1)
            : this.state.history.slice(this.state.history.length - this.state.stepNumber - 1);
        const current = history[this.state.sortAscending ? history.length - 1 : 0];
        const squares = current.squares.slice();  // create a copy to maintain immutability
        if (calculateWinner(squares) || squares[i]) {
            return; // return early if square is already clicked or we already have a winner.
        }
        squares[i] = this.state.xIsNext ? 'X' : 'O';
        const newStep = [{squares: squares, pos: i}];
        this.setState({  // use concat for immutability
            history: this.state.sortAscending ? history.concat(newStep) : newStep.concat(history),
            xIsNext: !this.state.xIsNext,
            stepNumber: history.length
        });
    }
    render() {
        const history = this.state.history;
        const current = history[this.state.sortAscending ? this.state.stepNumber : this.state.history.length - this.state.stepNumber - 1];
        const winnerResult = calculateWinner(current.squares);
        const sortAscending = this.state.sortAscending;

        const moves = history.map((step, move) => {  // here "move" is the index and step is the object
            const i = sortAscending ? move : history.length - move - 1;
            const desc = i ?
                'Go to move #' + i + ' @ (' + step.pos % 3 + ', ' + Math.floor(step.pos / 3) + ')':
                'Go to game start';
            const isCurrent = (move === history.indexOf(current));
            return ( // use move as a safe key because moves are always kept in order
                <li key={move}>
                    <button
                        onClick={() => this.jumpTo(i)}
                        style={{fontWeight: isCurrent ? 'bold' : ''}}  // bolds current move
                    >
                        {desc}
                    </button>
                </li>
            );
        });

        let status;
        if (winnerResult) {
            status = 'Winner: ' + winnerResult.winner;
        } else if (this.state.stepNumber === 9) {
            status = 'It\'s a draw.';
        } else {
            status = 'Next player: ' + (this.state.xIsNext ? 'X' : 'O');
        }
        return (
            <div className="game">
                <div className="game-board">
                    <Board
                        squares={current.squares}
                        highlight={winnerResult ? winnerResult.cells : []}
                        onClick={(i) => this.handleClick(i)}
                    />
                </div>
                <div className="game-info">
                    <div>{status}</div>
                    <button onClick={() => this.changeSort()}>Change sorting</button>
                    <ol>{moves}</ol>
                </div>
            </div>
        );
    }

    jumpTo(step) {
        this.setState({
            stepNumber: step,
            xIsNext: (step % 2) === 0,
        });
    }

    changeSort() {
        this.setState({
            history: this.state.history.slice().reverse(),
            sortAscending: !this.state.sortAscending
        });
    }
}

function calculateWinner(squares) {
    const lines = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [0, 3, 6],
        [1, 4, 7],
        [2, 5, 8],
        [0, 4, 8],
        [2, 4, 6],
    ];
    for (let i = 0; i < lines.length; i++) {
        const [a, b, c] = lines[i];
        if (squares[a] && squares[a] === squares[b] && squares[a] === squares[c]) {
            return {
                winner: squares[a],
                cells: lines[i]
            };
        }
    }
    return null;
}

// ========================================

ReactDOM.render(
    <Game/>,
    document.getElementById('root')
);

if (module.hot) {
    module.hot.accept();
}
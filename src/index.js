'use struct'

import './index.html';
import 'elm-canvas';

import { Main } from './Elm/Main.elm';
const main = document.getElementById('main');

const app = Main.embed(main);
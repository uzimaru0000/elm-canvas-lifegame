'use struct'

import './index.html';
import 'elm-canvas';

import { Elm } from './Elm/Main.elm';
const main = document.getElementById('main');

Elm.Main.init({ node: main })

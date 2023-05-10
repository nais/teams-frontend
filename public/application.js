import './main.scss';

import { Elm } from '../src/Main.elm'

const app = Elm.Main.init();

app.ports.copy.subscribe((text) => {
  navigator.clipboard.writeText(text)
});

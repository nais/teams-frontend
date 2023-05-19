import './main.scss';

import { Elm } from '../src/Main.elm'

const app = Elm.Main.init();

app.ports.copy.subscribe(text => {
  navigator.clipboard.writeText(text)
});

app.ports.openModal.subscribe(id => {
  const dialog = document.querySelector(`#${id}`)
  dialog.showModal()
});

app.ports.closeModal.subscribe(id => {
  const dialog = document.querySelector(`#${id}`)
  dialog.close()
});

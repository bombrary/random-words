exports.readBlobAsString = (blob) => () => {
  console.log(blob)
  const reader = new FileReader();
  reader.readAsText(blob);
  reader.onload = () => {
    console.log(reader.result)
  }
  return reader.result;
}

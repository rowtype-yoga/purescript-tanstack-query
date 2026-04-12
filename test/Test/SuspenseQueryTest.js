import React from 'react'

export const suspenseBoundary = ({ fallback, children }) =>
  React.createElement(React.Suspense, { fallback }, ...children)

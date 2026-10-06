"""Extract the original slide illustrations without rasterising whole pages.

Optional maintenance step; the extracted PNGs are included with the LaTeX source.
Requires PyMuPDF: python -m pip install pymupdf
"""

from pathlib import Path

import pymupdf


HERE = Path(__file__).resolve().parent
SOURCE = HERE / "Machine_Learning_Project.pdf"
ASSETS = (
    ("hsg_logo.png", 1, 0),
    ("disbursement_timeline.png", 3, 0),
    ("rf_tuning.png", 7, 0),
    ("rf_importance.png", 8, 0),
    ("logit_roc.png", 9, 0),
    ("logit_coefficients.png", 9, 1),
    ("nn_tuning.png", 10, 0),
    ("model_comparison.png", 11, 0),
)


def main():
    output = HERE / "figures"
    output.mkdir(exist_ok=True)
    with pymupdf.open(SOURCE) as document:
        if len(document) != 13:
            raise ValueError("Expected the original 13-page presentation PDF.")
        for name, page_number, image_number in ASSETS:
            images = document[page_number - 1].get_images(full=True)
            xref, smask = images[image_number][:2]
            image = pymupdf.Pixmap(document, xref)
            if smask:
                # Some PyMuPDF versions add an opaque alpha channel on load.
                # Apply the PDF's real soft mask explicitly (notably for the logo).
                if image.alpha:
                    image = pymupdf.Pixmap(image, 0)
                image = pymupdf.Pixmap(image, pymupdf.Pixmap(document, smask))
            if image.colorspace and image.colorspace.n > 3:
                image = pymupdf.Pixmap(pymupdf.csRGB, image)
            image.save(output / name)
            print(f"{name}: source page {page_number}, {image.width} x {image.height}")


if __name__ == "__main__":
    main()

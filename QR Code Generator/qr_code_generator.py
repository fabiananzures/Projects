import qrcode

def create_qr_code(url, qr_name):
    qr = qrcode.QRCode(
        version=1,
        error_correction=qrcode.constants.ERROR_CORRECT_M,
        box_size=10,
        border=4,
    )
    qr.add_data(url)
    qr.make(fit=True)
    img = qr.make_image(fill_color = 'black', back_color = 'white')
    img.save(qr_name)

url = 'http://linkedin.com/in/fabiananzures'
qr_name = 'fabian_linkedin.png'
create_qr_code(url, qr_name)

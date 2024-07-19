from flask import Flask, render_template, request, redirect, url_for, session
import hashlib, mysql.connector
from datetime import datetime
app = Flask(__name__)
app.secret_key = 'SECRET'
user_logins = mysql.connector.connect(user='', host='',database='')

@app.route("/login", methods = ['GET', 'POST'])
def login():
    msg=""
    if request.method == 'POST' and 'username' in request.form and 'password' in request.form:
        username = request.form['username']
        password = request.form['password']
        hashed_pass = password + app.secret_key
        hashed_pass = hashlib.sha1(hashed_pass.encode())
        hashed_pass = hashed_pass.hexdigest()        
        cursor = user_logins.cursor(buffered=True)
        cursor.execute('''SELECT * FROM users WHERE username= %s AND password=%s''', (username,hashed_pass,))
        account = cursor.fetchone()
        if account:
            session['loggedin'] = True
            session['id'] = account[0]
            session['username'] = account[1]
            todays_unformatted_date = datetime.today().now()
            todays_formatted_date = todays_unformatted_date.strftime('%Y-%m-%d')
            cursor = user_logins.cursor(buffered=True)
            cursor.execute('INSERT INTO invoices VALUES ((NULL), %s, %s)', (todays_formatted_date, session['id'],))
            user_logins.commit()
            cursor.execute('SELECT id FROM invoices ORDER BY id DESC LIMIT 1')    
            id = cursor.fetchone()[0]

            cursor.execute('SELECT * FROM invoice_content WHERE invoice_id = %s AND user_id = %s', (id,session['id'],))
            todays_services = cursor.fetchall()
            cursor.execute('SELECT name, lastname,company FROM users WHERE id = %s', (session['id'],))
            user_info = cursor.fetchall()
            cursor.execute('SELECT SUM(price) FROM invoice_content WHERE invoice_id = %s AND user_id = %s', (id,session['id'],))
            total = cursor.fetchone()[0]
            return render_template("create_invoice.html", todays_formatted_date=todays_formatted_date,id=id,total=total,user_info = user_info,todays_services = todays_services)
        else:
            msg = 'Could not sign in'
    return render_template('index.html', msg = msg)

@app.route('/login/register', methods = ['GET', 'POST'])
def register():
    msg = ''
    if request.method == 'POST' and 'username' in request.form and 'password' in request.form:
        name = request.form['name']
        lastname = request.form['lastname']
        username = request.form['username']
        company = request.form['company']
        password = request.form['password']
        cursor = user_logins.cursor(buffered=True)
        cursor.execute('SELECT * FROM users WHERE username = %s', (username,))
        account = cursor.fetchone()
        if account:
            msg='Account already registered'
        else:
            hash = password + app.secret_key
            hash = hashlib.sha1(hash.encode())
            password = hash.hexdigest()
            cursor.execute('INSERT INTO users VALUES ((NULL), %s, %s,%s,%s,%s)', (name, lastname,company,username,password,))
            user_logins.commit()
            msg = 'You have registered!'
    return render_template('register.html', msg=msg)

@app.route('/login/create_invoice', methods=['POST', 'GET'])
def create_invoice():
    if 'loggedin'in session:
        todays_unformatted_date = datetime.today().now()
        todays_formatted_date = todays_unformatted_date.strftime('%Y-%m-%d')
        if request.method == "POST":
            date = request.form['date']
            service = request.form['service']
            price = request.form['price']
            cursor = user_logins.cursor(buffered=True)
            cursor.execute('SELECT id FROM invoices ORDER BY id DESC LIMIT 1')    
            id = cursor.fetchone()[0]
            cursor.execute('''INSERT INTO invoice_content VALUES ((NULL),%s,%s,%s,%s,%s,%s)''', (todays_formatted_date,date,service,price,session['id'],id,))
            user_logins.commit()
            return redirect('/login/create_invoice')
        else:    
            cursor = user_logins.cursor(buffered=True)
            cursor.execute('SELECT id FROM invoices ORDER BY id DESC LIMIT 1')    
            id = cursor.fetchone()[0]

            cursor.execute('SELECT * FROM invoice_content WHERE invoice_id = %s AND user_id = %s', (id,session['id'],))
            todays_services = cursor.fetchall()
            cursor.execute('SELECT name, lastname FROM users WHERE id = %s', (session['id'],))
            user_info = cursor.fetchall()
            cursor.execute('SELECT SUM(price) FROM invoice_content WHERE invoice_id = %s AND user_id = %s', (id,session['id'],))
            total = cursor.fetchone()[0]
            return render_template('create_invoice.html', todays_formatted_date=todays_formatted_date,id = id,total=total, user_info = user_info,todays_services=todays_services)
    else:
        return redirect('/login')
@app.route('/delete/<int:id>')
def delete(id):
    cursor = user_logins.cursor(buffered=True)
    cursor.execute('DELETE FROM invoice_content WHERE id = %s', (id,))
    user_logins.commit()
    return redirect('/login/create_invoice')
@app.route('/login/logout')
def logout():
    session.pop('loggedin', None)
    session.pop('id', None)
    session.pop('account', None)
    return redirect('/login')
if __name__ == '__main__':
    app.run(debug=True)
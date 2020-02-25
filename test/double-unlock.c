int lock1;
int lock2;

void extern _spin_unlock(void *);

void double_unlock(int flag)
{
	if (flag) {
		_spin_unlock(&lock1);
	}
	else {
		_spin_unlock(&lock2);
	}
}

int main()
{
	double_unlock(1);
	// _spin_unlock(&lock1);
	_spin_unlock(&lock1);
}